#include <stdbool.h>
#include <stdlib.h>

typedef struct List list_t;
struct List {
    void * head;
    list_t * tail;
};

list_t * makelist (void * x, list_t * xs){
    list_t * ans = (list_t *)malloc(sizeof(list_t));
    ans->head = x;
    ans->tail = xs;
    return ans;
}

list_t* map(void* (*f)(void*,void*), void* env, list_t* xs) {
    if(xs==NULL){
        return NULL;
    }
    return makelist(f(env,xs->head), map(f,env,xs->tail));
}

list_t* filter(bool (*f)(void*, void*), void* env, list_t* xs) {
    if(xs==NULL){
        return NULL;
    }
    if(f(env, xs->head)){
        return makelist(xs->head, filter(f, env, xs->tail));
    } else {
        return filter(f, env, xs->tail);
    }
}

int length(list_t* xs) {
    int ans = 0;
    while(xs != NULL) {
        ++ans;
        xs = xs->tail;
    }
    return ans;
}

void* doubleInt(void* ignore, void* i) {
    return (void*)(((__intptr_t)i)*2);
}

list_t* doubleAll(list_t* xs) {
    return map(doubleInt, NULL, xs);
}

bool isN(void* n, void* i) {
    return ((__intptr_t)n)==((__intptr_t)i);
}

int countNs(list_t* xs, __intptr_t n) {
    return length(filter(isN, (void*)n, xs));
}
