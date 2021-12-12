#include <pthread.h>
#include <stdio.h>

void *start_routine(void* args) {
    long a = 30;
    long b = 10;
    void *x = (void *)(a+b);
    return x;
}


int start_routine_r() {
    printf("Yolo\t\n");
    return 16;
}

int main() {
    pthread_t t;
    int a = 30;
    pthread_create(&t, NULL, start_routine, &a);
    void *b;
    pthread_join(t, &b);
    printf("Et b = %d\n", (int)b);
}