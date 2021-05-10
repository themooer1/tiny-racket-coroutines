#include <assert.h>
#include <stdio.h>
#include <sys/mman.h>

int main(void) {
    int *p;

    if (MAP_FAILED == (p = mmap(NULL, 5 * sizeof(int), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0))) {
        perror("Mapping failed: ");
    }
     
     for (int i = 0; i < 5; ++i) {
        p[i] = i;
    }
    
    for (int i = 0; i < 5; ++i) {
        assert(p[i] == i);
    }

    if (-1 == (munmap(p, 5 * sizeof(int)))) {
        perror("Unmapping failed: ");
    }
}