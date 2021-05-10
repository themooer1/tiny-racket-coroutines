#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include "coroutine.h"
#include "types.h"
#include "runtime.h"

FILE* in;
FILE* out;
void (*error_handler)();
int64_t *heap;

void print_result(int64_t);

void error_exit() {
  printf("err\n");
  exit(1);
}

void raise_error() {
  return error_handler();
}

int main(int argc, char** argv) {
  in = stdin;
  out = stdout;
  error_handler = &error_exit;
  heap = malloc(8 * heap_size);

  // Setup coroutines
  cr_init();
  cr_main(heap);
  puts("cr_main shouldn't return!");

  // struct coroutine *entry_cr = cr_create(NULL, NULL);


  // int64_t result = entry(heap, entry_cr->stack.rsp);
  // See if we need to print the initial tick  if (cons_type_tag == (ptr_type_mask & result)) printf("'");
  // puts("Exiting through main is now discouraged :)");
  // print_result(result);
  // if (result != val_void) printf("\n");
  // free(heap);
  return 0;

}


int quit(int64_t result) {
  // uint64_t padding;
  // puts("The coroutine returned : >");

  if (cons_type_tag == (ptr_type_mask & result)) printf("'");
  print_result(result);
  if (result != val_void) printf("\n");
  // free(heap);
  exit(0);
  return 0;
}



// int64_t cr_entry(int64_t lambda, void *stack_ptr);

void print_char(int64_t);
void print_cons(int64_t);

void print_result(int64_t result) {
  if (cons_type_tag == (ptr_type_mask & result)) {
    printf("(");
    print_cons(result);
    printf(")");
  } else if (box_type_tag == (ptr_type_mask & result)) {
    printf("#&");
    print_result (*((int64_t *)(result ^ box_type_tag)));
  } else if (proc_type_tag == (ptr_type_mask & result)) {
    printf("<procedure @ %lu>", result ^ proc_type_tag);
    // struct coroutine *cr = cr_create(result, NULL);
    // puts("CrCr");
    // int64_t r2 = cr_entry(result, cr->stack.rsp);
    //      ^^ not necessary cr doesn't return it calls quit
    // puts("Coroutine should not return!");
    // print_result(r2);
  } else if (int_type_tag == (int_type_mask & result)) {
    printf("%" PRId64, result >> int_shift);
  } else if (char_type_tag == (char_type_mask & result)) {
    print_char(result);
  } else {
    switch (result) {
    case val_true:
      printf("#t"); break;
    case val_false:
      printf("#f"); break;
    case val_eof:
      printf("#<eof>"); break;
    case val_empty:
      printf("()"); break;
    case val_void:
      /* nothing */ break;
    }
  }  
}

void print_cons(int64_t a) {  
  int64_t car = *((int64_t *)((a + 8) ^ cons_type_tag));
  int64_t cdr = *((int64_t *)((a + 0) ^ cons_type_tag));
  print_result(car);
  if (cdr == val_empty) {
    // nothing
  } else if (cons_type_tag == (ptr_type_mask & cdr)) {
    printf(" ");
    print_cons(cdr);
  } else {
    printf(" . ");
    print_result(cdr);
  }
}
