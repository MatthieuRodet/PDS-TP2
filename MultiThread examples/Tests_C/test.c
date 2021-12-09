#include <pthread.h>
#include <stdio.h>

void *start_routine(void* args) {
    return args;
}


void start_routine_r() {
    printf("Yolo\t\n");
}

int main() {
    pthread_t t;
    int a = 30;
    pthread_create(&t, NULL, start_routine, &a);
    pthread_join(t, NULL);
}