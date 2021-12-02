#include <pthread.h>

void *start_routine(void* args) {
    return args;
}

int main() {
    pthread_t t;
    pthread_create(&t, NULL, start_routine, NULL);
    pthread_join(t, NULL);
}