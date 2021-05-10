#ifndef COROUTINE_H
#define COROUTINE_H

#include <stdbool.h>
#include <time.h>


enum cr_status {
    STARTING,
    RUNNING,
    FINISHED,
    FAILED
};

// Why a coroutine can't be scheduled
enum block_reason {
    NONE = 0,
    CHILD = 1,
    IO = 2,
    SLEEP = 3
};

// Blocked waiting for a child
struct child_block_state {
    uint32_t waiting_on;  // Number of children this cr is waiting on
};

// Blocked waiting for IO
struct io_block_state {

};

// Blocked sleeping
struct sleep_block_state {
    time_t wake_up;  // When to wake from sleep
};


// If and why a coroutine is blocked
struct block_state {
    bool blocked;
    enum block_reason reason;
    union {
       struct child_block_state child;
       struct io_block_state io;
       struct sleep_block_state sleep; 
    } state;
};


struct cr_stack {
    void *stack;
    uint64_t size;
    void *rsp;
};

struct coroutine{
    struct block_state block_state;
    enum cr_status status;
    int64_t lambda;

    struct coroutine *next;
    struct coroutine *parent;

    struct cr_stack stack;
    void *rip;

    int64_t result;

};

/**
 *  Must be called to setup coroutines.
 */
void cr_init();

/**
 * Starts entry as the first coroutine
 */
void cr_main(void *heap);

struct coroutine *cr_create(int64_t lambda, struct coroutine *parent);
// struct coroutine *cr_start(int64_t lambda);
void cr_make_schedulable(struct coroutine *c);
void cr_free(struct coroutine *c);

// Defined by compiled program
extern void cr_entry(int64_t lambda, void *stack);
extern void cr_resume(void *saved_rsp, void *saved_rip);

// Called from compiled program
void cr_gather(int64_t num_lambdas, ...);
void cr_quit(int64_t result);
void cr_schedule(void);
void cr_yield(void *saved_rsp);


#endif

