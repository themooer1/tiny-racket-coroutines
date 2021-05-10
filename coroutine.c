#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

#include "coroutine.h"
#include "runtime.h"
#include "types.h"

#ifndef CR_DEBUG
#define CR_DEBUG 1  // Levels 0, 1, 2
#endif

// Current CoRoutine
#define CCR (current)
// #define NUM_CRS (sizeof(coroutines)/sizeof(coroutines[0]))

#define COROUTINE_MAX_STACK_SIZE (1024 * PAGE_SIZE)
#define COROUTINE_MIN_STACK_SIZE (1 * PAGE_SIZE)
static uint32_t PAGE_SIZE = 0;

#define STACK_ALIGN_POWER 4 /* 16 byte alignment */

/* Circular list of coroutines */
static struct coroutine *current = NULL;

/* Pointer to stack used for managing coroutines */
static void *global_rsp = NULL;


// From main.c  Prints result and exits
extern int quit(int64_t result);


static struct cr_stack cr_alloc_stack();
static void cr_free_stack(struct coroutine *c);

static struct coroutine *cr_create_entry_coroutine() {
    struct coroutine *entry_coroutine = cr_create(0L, NULL);
    /* 
    Entry cr doesn't run a lambda, it just runs code from the normal entrypoint.
    We don't want schedule trying to start it as a lambda, so we mark it
    RUNNING so schedule just resumes it.
    */
    entry_coroutine->status = RUNNING;

}

static void cr_init_schedule_list() {
    current = cr_create_entry_coroutine();
}

void cr_init() {
    PAGE_SIZE = sysconf(_SC_PAGESIZE);
    if (CR_DEBUG >= 2)
        printf("PAGE_SIZE=%d\n", PAGE_SIZE);
    cr_init_schedule_list();
}

void cr_main(void *heap) {
    asm("movq %%rsp, %0;"
        :"=r"(global_rsp));
    printf("Unaligned Saved RSP: %p\n", global_rsp);
    global_rsp = (void *) ((uint64_t) global_rsp & -16);
    printf("Saved RSP: %p\n", global_rsp);

    entry(heap, CCR->stack.rsp);
}


static struct cr_stack cr_alloc_stack() {
    struct cr_stack rval;
    void *stack;

    stack = mmap(NULL, COROUTINE_MIN_STACK_SIZE, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (stack == MAP_FAILED) {
        perror("Failed to alloc coroutine stack: ");
        abort();
    }


    rval.size = COROUTINE_MIN_STACK_SIZE;
    rval.stack = stack;
    rval.rsp = rval.stack + COROUTINE_MIN_STACK_SIZE - 8;

    if (CR_DEBUG >= 2)
        printf("Mapped cr stack:\n\t stack: %p\n\trsp: %p\n\tsize: %lu\n", rval.stack, rval.rsp, rval.size);

    // printf("Unmapping cr stack:\n\t stack: %p\n\trsp: %p\n\tsize: %lu\n", rval.stack, rval.rsp, rval.size);
    // munmap(rval.stack, rval.size);
    // puts("unmap worked");
    // abort();
    return rval;
};

/**
 * Creates a coroutine, with a stack and entry point
 * 
 * @param entry_fun ptr to function which this coroutine will run
 * 
 */
struct coroutine *cr_create(int64_t lambda, struct coroutine *parent) {
    struct coroutine *c = malloc(sizeof *c);

    *c = (struct coroutine) {
        .block_state = (struct block_state) {
            .blocked = false,
            .reason = NONE,
            .state = {0}
        },
        .next = c,
        .parent = parent,
        .lambda = lambda,

        .stack = cr_alloc_stack(),
        .rip = NULL,

        .result = 0

    };

    return c;
}

// struct coroutine *cr_create_raw(void *entry_point, struct coroutine *parent) {
//     struct coroutine *c = cr_create(0L, parent);
//     c.rip
// }

// struct coroutine *cr_create_from_lambda(int64_t lambda) {
//     struct coroutine *c = cr_create(CCR);
// }


static void cr_free_stack(struct coroutine *c) {
    if (CR_DEBUG >= 2)
        printf("Unmapping cr stack:\n\t stack: %p\n\trsp: %p\n\tsize: %lu\n", c->stack.stack, c->stack.rsp, c->stack.size);

    if (-1 == munmap(c->stack.stack, c->stack.size)) {
        perror("Unmapping cr stack failed!");
        abort();
    }
}

void cr_free(struct coroutine *c) {
    cr_free_stack(c);  // Probably already freed by cr_quit
    free(c);
}

static void cr_quit_2(int64_t result) {
    CCR->result = result;
    cr_free_stack(CCR);


    if (CCR->parent) {
        // Save our return value
        // For now we just print it
        print_result(CCR->result);
        if (CCR->result != val_void) putchar('\n');

        // Prevent scheduling this CR while we wait for parent to get the return val
        CCR->status = FINISHED;

        // Decrement children parent is waiting for
        assert(CCR->parent->block_state.blocked == true);
        assert(CCR->parent->block_state.reason == CHILD);
        assert(CCR->parent->block_state.state.child.waiting_on > 0);

        --CCR->parent->block_state.state.child.waiting_on;

        if (CCR->parent->block_state.state.child.waiting_on == 0) {
            // The parent gets to run!  YAY

            if (CR_DEBUG >= 1)
                puts("Parent unblocked!");
            
            CCR->parent->block_state.blocked = false;
        }

        cr_schedule();
    }

    else {
        // Quit the program when original coroutine quits
        // Like init process ending

        puts("THANKS FOR YOUR HELP! :)");
        quit(result);
    }
}

/**
 * Like exit for coroutines.
 * Collects the return value of a coroutine and frees its stack.
 * Coroutine struct PERSISTS (like a zombie process) until parent gets return value
 * 
 * @param result return value of the coroutine
 */
void cr_quit(int64_t result) {
    register void *sp asm ("rsp") = global_rsp;
    // asm volatile("movq %0, %%rsp;"
    //     ::"r"(global_rsp)
    //     :);

    if (CR_DEBUG >= 2)
        puts("cr_quit");

    cr_quit_2(result);
}


extern void cr_resume(void *saved_rsp, void *saved_rip);
void cr_yield(void *saved_rsp) {

    CCR->stack.rsp = saved_rsp;
    CCR->rip = __builtin_return_address(0);

    cr_schedule();
}


void cr_make_schedulable(struct coroutine *c) {
    c->next = CCR->next;
    CCR->next = c;
}

void cr_schedule(void) {
    // For now just resume the current coroutine
    
    if (CR_DEBUG >= 1)
        puts("Scheduling...  -_-_-_-_-_-_-_");

    do {
        // Failed coroutine should be immediately removed from the schd list
        assert(CCR->status != FAILED);

        CCR = CCR->next;
    } while (CCR->block_state.blocked || CCR->status == FINISHED);

    switch (CCR->status) {
        case STARTING:
            CCR->status = RUNNING;
            cr_entry(CCR->lambda, CCR->stack.rsp);
            break;

        case RUNNING:
            cr_resume(CCR->stack.rsp, CCR->rip);
            break;

    }

}

/**
 * Load length prefixed list of lambdas off the stack
 * and make a coroutine for each.
 * 
 * This function stays on calling CR's stack
 */
void __attribute__((__cdecl__)) cr_gather(int64_t num_lambdas, ...) {
    puts("cr_gather");
    printf("Starting %lu coroutines.\n", num_lambdas);
    if (num_lambdas > 0) {
        va_list lambdas;
        va_start(lambdas, num_lambdas);

        // Block CCR until all children finish
        CCR->block_state = (struct block_state) {
            .blocked = true,
            .reason = CHILD,
            .state = (struct child_block_state) {
                .waiting_on = num_lambdas
            }
        };

        // Make CRs from all lambdas and add them to the SCHD list
        while (num_lambdas-- > 0) {
            int64_t lambda = va_arg(lambdas, int64_t);
            struct coroutine *cr = cr_create(lambda, CCR);
            cr_make_schedulable(cr);
        }
    }

    // Caller should immediately yield after calling gather!

}