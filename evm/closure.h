#ifndef _CLOSURE_H
#define _CLOSURE_H

#include <gc/gc.h>
#include <stdio.h>

#define EMALLOC GC_MALLOC
#define EREALLOC GC_REALLOC

#define MKCON (con*)EMALLOC(sizeof(con))
#define MKFUN (fun*)EMALLOC(sizeof(fun))
#define MKTHUNK (thunk*)EMALLOC(sizeof(thunk))
#define MKCLOSURE (Closure*)EMALLOC(sizeof(Closure))
#define MKUNIT (void*)0

#define INTOP(op,x,y) MKINT((int)(((VAL)x)->info) op (int)(((VAL)y)->info))
#define CHECKEVALUATED(x) if(((VAL)x)->ty==FUN || ((VAL)x)->ty==THUNK \
    || ((VAL)x)->ty==FREEVAR) return 0;

#define MKARGS(x) (void**)EMALLOC(sizeof(Closure)*x);
#define MOREARGS(args,x) (void**)EREALLOC(args,sizeof(Closure)*x);

typedef enum { 
    FUN, 
    THUNK, 
    CON, 
    INT, 
    FLOAT, 
    STRING, 
    UNIT, 
    FREEVAR 
} ClosureType;

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
    void* fn;
    void** args;
    int numargs;
} thunk;

typedef struct {
    int tag;
    void** args;
} con;

#define UPDATE(x,res) x->ty = res->ty; x->info=res->info;
#define TAG(x) ((con*)((Closure*)x)->info)->tag

#define ISCON(x) ((Closure*)x)->ty==CON
#define ISINT(x) ((Closure*)x)->ty==INT

// Evaluate x to head normal form
void EVAL(VAL x);

// Return a new constructor
VAL CONSTRUCTOR(int tag, int arity, void** block);

// Return a new function node
VAL CLOSURE(func x, int arity, int args, void** block);

// Add arguments to an already existing thunk
VAL CLOSURE_ADDN(VAL x, int args, void** block);

// Apply a closure to some arguments
VAL CLOSURE_APPLY(VAL x, int args, void** block);

// Project an argument from a constructor
void* PROJECT(VAL x, int arg);

// Create new primitive values
void* MKINT(int x);
void* MKSTR(char* str);
// Get an integer from a closure
int GETINT(void* x);
char* GETSTR(void* x);

void* MKFREE(int x);

// Exit with fatal error
void ERROR(char* msg);

// Some basic communication with the outside world

void putStr(char* str);
void printInt(int x);

int readInt();
char* readStr();

int strToInt(char* str);
char* intToStr(int x);

#endif
