#ifndef _CLOSURE_H
#define _CLOSURE_H

#include <gc/gc.h>
#include <malloc.h>

#define EMALLOC GC_MALLOC
#define EREALLOC GC_REALLOC

#define MKCON (con*)EMALLOC(sizeof(con))
#define MKFUN (fun*)EMALLOC(sizeof(fun))
#define MKCLOSURE (Closure*)EMALLOC(sizeof(Closure))
#define MKUNIT (void*)0

#define INTOP(op,x,y) MKINT((int)(((VAL)x)->info) op (int)(((VAL)y)->info))

#define MKARGS(x) (void**)EMALLOC(sizeof(Closure)*x);
#define MOREARGS(args,x) (void**)EREALLOC(args,sizeof(Closure)*x);

typedef enum { FUN, CON, INT, FLOAT, STRING, UNIT } ClosureType;

typedef struct {
    ClosureType ty;
    void* info;
} Closure;

typedef Closure* VAL;

typedef void*(*func)(void**);

typedef struct {
    func fn;
    void** args;
    void** arg_end;
    int arity;
} fun;

typedef struct {
    int tag;
    void** args;
} con;

#define UPDATE(x,res) *x = res; // x->ty = res->ty; x->info=res->info;
#define TAG(x) ((con*)((Closure*)x)->info)->tag

#define ISCON(x) ((Closure*)x)->ty==CON

// Evaluate x to head normal form
void EVAL(void** x);

// Return a new constructor
VAL CONSTRUCTOR(int tag, int arity, void** block);

// Return a new thunk
VAL CLOSURE(func x, int arity, int args, void** block);

// Add arguments to an already existing thunk
VAL CLOSURE_ADDN(VAL x, int args, void** block);

// Project an argument from a constructor
void* PROJECT(VAL x, int arg);

// Create a new integer
void* MKINT(int x);
// Get an integer from a closure
int GETINT(void* x);

void printInt(int x);


#endif