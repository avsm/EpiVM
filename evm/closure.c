#include "closure.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>

VAL one;

inline VAL CLOSURE(func x, int arity, int args, void** block)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(fun)); // MKCLOSURE;
    fun* fn = (fun*)(c+1);
    fn->fn = x;
    fn->arity = arity;
    if (args==0) {
	fn->args = 0;
	fn->arg_end = 0;
    } else {
	fn->args = MKARGS(args);
	fn->arg_end=fn->args+args;
	memcpy((void*)(fn->args), (void*)block, args*sizeof(VAL));
    }

    SETTY(c, FUN);
    c->info = (void*)fn;
    return c;
}

inline VAL CONSTRUCTOR(int tag, int arity, void** block)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(con)); // MKCLOSURE;
    con* cn = (con*)(c+1);
    cn->tag = tag;
    if (arity==0) {
	cn->args = 0;
    } else {
	cn->args = MKARGS(arity);
	memcpy((void*)(cn->args), (void*)block, arity*sizeof(VAL));
    }
    SETTY(c, CON);
    c->info = (void*)cn;
    return c;
}

inline VAL CONSTRUCTOR1(int tag, VAL a1)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(con)); // MKCLOSURE;
    con* cn = (con*)(c+1);
    cn->tag = tag;
    cn->args = MKARGS(1);
    cn->args[0] = a1;
    SETTY(c,CON);
    c->info = (void*)cn;
    return c;
}

inline VAL CONSTRUCTOR2(int tag, VAL a1, VAL a2)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(con)); // MKCLOSURE;
    con* cn = (con*)(c+1);
    cn->tag = tag;
    cn->args = MKARGS(2);
    cn->args[0] = a1;
    cn->args[1] = a2;
    SETTY(c,CON);
    c->info = (void*)cn;
    return c;
}

inline VAL CONSTRUCTOR3(int tag, VAL a1, VAL a2, VAL a3)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(con)); // MKCLOSURE;
    con* cn = (con*)(c+1);
    cn->tag = tag;
    cn->args = MKARGS(3);
    cn->args[0] = a1;
    cn->args[1] = a2;
    cn->args[2] = a3;
    SETTY(c,CON);
    c->info = (void*)cn;
    return c;
}

inline VAL CONSTRUCTOR4(int tag, VAL a1, VAL a2, VAL a3, VAL a4)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(con)); // MKCLOSURE;
    con* cn = (con*)(c+1);
    cn->tag = tag;
    cn->args = MKARGS(2);
    cn->args[0] = a1;
    cn->args[1] = a2;
    cn->args[2] = a3;
    cn->args[3] = a4;
    SETTY(c,CON);
    c->info = (void*)cn;
    return c;
}

inline VAL CONSTRUCTOR5(int tag, VAL a1, VAL a2, VAL a3, VAL a4, VAL a5)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(con)); // MKCLOSURE;
    con* cn = (con*)(c+1);
    cn->tag = tag;
    cn->args = MKARGS(5);
    cn->args[0] = a1;
    cn->args[1] = a2;
    cn->args[2] = a3;
    cn->args[3] = a4;
    cn->args[4] = a5;
    SETTY(c,CON);
    c->info = (void*)cn;
    return c;
}

// This needs to make a copy
inline VAL CLOSURE_ADDN(VAL xin, int args, void** block)
{
    assert(GETTY(xin) == FUN);

    fun* finf = (fun*)xin->info;

    VAL x = CLOSURE(finf->fn, finf->arity, 
		    finf->arg_end-finf->args, finf->args);

    fun* fn = (fun*)(x->info);
    int diff = fn->arg_end - fn->args;

    fn->args = MOREARGS(fn->args, args);
    fn->arg_end = fn->args + diff;

    memcpy((void*)(fn->arg_end), (void*)block, args*sizeof(VAL));
    fn->arg_end += args;
    return x;
}

/*
VAL CLOSURE_ADDN(VAL xin, int args, void** block)
{
    switch(args) {
    case 1: return CLOSURE_ADD1(xin,block[0]);
    case 2: return CLOSURE_ADD2(xin,block[0],block[1]);
    case 3: return CLOSURE_ADD3(xin,block[0],block[1],block[2]);
    case 4: return CLOSURE_ADD4(xin,block[0],block[1],block[2],block[3]);
    case 5: return CLOSURE_ADD5(xin,block[0],block[1],block[2],block[3],block[4]);
    default: return aux_CLOSURE_ADDN(xin,args,block);
    }
}
*/

inline VAL CLOSURE_ADD1(VAL xin, VAL a1)
{
    assert(GETTY(xin)==FUN);

    fun* finf = (fun*)xin->info;

    VAL x = CLOSURE(finf->fn, finf->arity, 
		    finf->arg_end-finf->args, finf->args);

    fun* fn = (fun*)(x->info);
    int diff = fn->arg_end - fn->args;

    fn->args = MOREARGS(fn->args, 1);
    fn->arg_end = fn->args + diff;
    fn->arg_end[0] = a1;
    fn->arg_end+=1;

    return x;
}

inline VAL CLOSURE_ADD2(VAL xin, VAL a1, VAL a2)
{
    assert(GETTY(xin)==FUN);

    fun* finf = (fun*)xin->info;

    VAL x = CLOSURE(finf->fn, finf->arity, 
		    finf->arg_end-finf->args, finf->args);

    fun* fn = (fun*)(x->info);
    int diff = fn->arg_end - fn->args;

    fn->args = MOREARGS(fn->args, 2);
    fn->arg_end = fn->args + diff;
    fn->arg_end[0] = a1;
    fn->arg_end[1] = a2;
    fn->arg_end+=2;

    return x;
}

inline VAL CLOSURE_ADD3(VAL xin, VAL a1, VAL a2, VAL a3)
{
    assert(GETTY(xin)==FUN);

    fun* finf = (fun*)xin->info;

    VAL x = CLOSURE(finf->fn, finf->arity, 
		    finf->arg_end-finf->args, finf->args);

    fun* fn = (fun*)(x->info);
    int diff = fn->arg_end - fn->args;

    fn->args = MOREARGS(fn->args, 3);
    fn->arg_end = fn->args + diff;
    fn->arg_end[0] = a1;
    fn->arg_end[1] = a2;
    fn->arg_end[2] = a3;
    fn->arg_end+=3;

    return x;
}

inline VAL CLOSURE_ADD4(VAL xin, VAL a1, VAL a2, VAL a3, VAL a4)
{
    assert(GETTY(xin)==FUN);

    fun* finf = (fun*)xin->info;

    VAL x = CLOSURE(finf->fn, finf->arity, 
		    finf->arg_end-finf->args, finf->args);

    fun* fn = (fun*)(x->info);
    int diff = fn->arg_end - fn->args;

    fn->args = MOREARGS(fn->args, 2);
    fn->arg_end = fn->args + diff;
    fn->arg_end[0] = a1;
    fn->arg_end[1] = a2;
    fn->arg_end[2] = a3;
    fn->arg_end[3] = a4;
    fn->arg_end+=4;

    return x;
}

inline VAL CLOSURE_ADD5(VAL xin, VAL a1, VAL a2, VAL a3, VAL a4, VAL a5)
{
    assert(GETTY(xin)==FUN);

    fun* finf = (fun*)xin->info;

    VAL x = CLOSURE(finf->fn, finf->arity, 
		    finf->arg_end-finf->args, finf->args);

    fun* fn = (fun*)(x->info);
    int diff = fn->arg_end - fn->args;

    fn->args = MOREARGS(fn->args, 2);
    fn->arg_end = fn->args + diff;
    fn->arg_end[0] = a1;
    fn->arg_end[1] = a2;
    fn->arg_end[2] = a3;
    fn->arg_end[3] = a4;
    fn->arg_end[4] = a5;
    fn->arg_end+=2;

    return x;
}

inline VAL CLOSURE_APPLY(VAL f, int args, void** block)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(thunk)); // MKCLOSURE;
    thunk* fn = (thunk*)(c+1);

    if (ISFUN(f)) {
	return CLOSURE_ADDN(f,args,block);
    }

    fn->fn = (void*)f;
    fn->numargs = args;
    if (args==0) {
	fn->args = 0;
    } else {
	fn->args = MKARGS(args);
	memcpy((void*)(fn->args), (void*)block, args*sizeof(VAL));
    }

    SETTY(c,THUNK);
    c->info = (void*)fn;
    return c;
}

inline VAL aux_CLOSURE_APPLY1(VAL f, VAL a1)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(thunk)); // MKCLOSURE;
    thunk* fn = (thunk*)(c+1);

    if (ISFUN(f)) {
	return CLOSURE_ADD1(f,a1);
    }

    fn->fn = (void*)f;
    fn->numargs = 1;
    fn->args = MKARGS(1);
    fn->args[0] = a1;

    SETTY(c,THUNK);
    c->info = (void*)fn;
    return c;
}

inline VAL aux_CLOSURE_APPLY2(VAL f, VAL a1, VAL a2)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(thunk)); // MKCLOSURE;
    thunk* fn = (thunk*)(c+1);

    if (ISFUN(f)) {
	return CLOSURE_ADD2(f,a1,a2);
    }

    fn->fn = (void*)f;
    fn->numargs = 2;
    fn->args = MKARGS(2);
    fn->args[0] = a1;
    fn->args[1] = a2;

    SETTY(c,THUNK);
    c->info = (void*)fn;
    return c;
}

inline VAL aux_CLOSURE_APPLY3(VAL f, VAL a1, VAL a2, VAL a3)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(thunk)); // MKCLOSURE;
    thunk* fn = (thunk*)(c+1);

    if (ISFUN(f)) {
	return CLOSURE_ADD3(f,a1,a2,a3);
    }

    fn->fn = (void*)f;
    fn->numargs = 2;
    fn->args = MKARGS(2);
    fn->args[0] = a1;
    fn->args[1] = a2;
    fn->args[2] = a3;

    SETTY(c,THUNK);
    c->info = (void*)fn;
    return c;
}

inline VAL aux_CLOSURE_APPLY4(VAL f, VAL a1, VAL a2, VAL a3, VAL a4)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(thunk)); // MKCLOSURE;
    thunk* fn = (thunk*)(c+1);

    if (ISFUN(f)) {
	return CLOSURE_ADD4(f,a1,a2,a3,a4);
    }

    fn->fn = (void*)f;
    fn->numargs = 2;
    fn->args = MKARGS(2);
    fn->args[0] = a1;
    fn->args[1] = a2;
    fn->args[2] = a3;
    fn->args[3] = a4;

    SETTY(c,THUNK);
    c->info = (void*)fn;
    return c;
}

inline VAL aux_CLOSURE_APPLY5(VAL f, VAL a1, VAL a2, VAL a3, VAL a4, VAL a5)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(thunk)); // MKCLOSURE;
    thunk* fn = (thunk*)(c+1);

    if (ISFUN(f)) {
	return CLOSURE_ADD5(f,a1,a2,a3,a4,a5);
    }

    fn->fn = (void*)f;
    fn->numargs = 2;
    fn->args = MKARGS(2);
    fn->args[0] = a1;
    fn->args[1] = a2;
    fn->args[2] = a3;
    fn->args[3] = a4;
    fn->args[4] = a5;

    SETTY(c,THUNK);
    c->info = (void*)fn;
    return c;
}

inline VAL CLOSURE_APPLY1(VAL f, VAL a1)
{
    if (ISFUN(f)) {
	fun* finf = (fun*)(f->info);
	int got = finf->arg_end-finf->args;
	if (finf->arity == (got+1)) {
	    void*block[1];
	    memcpy(block, finf->args, got*sizeof(VAL));
	    block[got] = a1;
	    return (VAL)(finf->fn(block));
	}
	else return CLOSURE_ADD1(f,a1);
    }
    else return aux_CLOSURE_APPLY1(f,a1);
}

inline VAL CLOSURE_APPLY2(VAL f, VAL a1, VAL a2)
{
    if (ISFUN(f)) {
	fun* finf = (fun*)(f->info);
	int got = finf->arg_end-finf->args;
	if (finf->arity == (got+2)) {
	    void*block[2];
	    memcpy(block, finf->args, got*sizeof(VAL));
	    block[got] = a1;
	    block[got+1] = a2;
	    return (VAL)(finf->fn(block));
	}
	else return CLOSURE_ADD2(f,a1,a2);
    }
    else return aux_CLOSURE_APPLY2(f,a1,a2);
}

inline VAL CLOSURE_APPLY3(VAL f, VAL a1, VAL a2, VAL a3)
{
    if (ISFUN(f)) {
	fun* finf = (fun*)(f->info);
	int got = finf->arg_end-finf->args;
	if (finf->arity == (got+3)) {
	    void*block[3];
	    memcpy(block, finf->args, got*sizeof(VAL));
	    block[got] = a1;
	    block[got+1] = a2;
	    block[got+2] = a3;
	    return (VAL)(finf->fn(block));
	}
	else return CLOSURE_ADD3(f,a1,a2,a3);
    }
    else return aux_CLOSURE_APPLY3(f,a1,a2,a3);
}

inline VAL CLOSURE_APPLY4(VAL f, VAL a1, VAL a2, VAL a3, VAL a4)
{
    if (ISFUN(f)) {
	fun* finf = (fun*)(f->info);
	int got = finf->arg_end-finf->args;
	if (finf->arity == (got+4)) {
	    void*block[4];
	    memcpy(block, finf->args, got*sizeof(VAL));
	    block[got] = a1;
	    block[got+1] = a2;
	    block[got+2] = a3;
	    block[got+3] = a4;
	    return (VAL)(finf->fn(block));
	}
	else return CLOSURE_ADD4(f,a1,a2,a3,a4);
    }
    else return aux_CLOSURE_APPLY4(f,a1,a2,a3,a4);
}

inline VAL CLOSURE_APPLY5(VAL f, VAL a1, VAL a2, VAL a3, VAL a4, VAL a5)
{
    if (ISFUN(f)) {
	fun* finf = (fun*)(f->info);
	int got = finf->arg_end-finf->args;
	if (finf->arity == (got+5)) {
	    void*block[5];
	    memcpy(block, finf->args, got*sizeof(VAL));
	    block[got] = a1;
	    block[got+1] = a2;
	    block[got+2] = a3;
	    block[got+3] = a4;
	    block[got+4] = a5;
	    return (VAL)(finf->fn(block));
	}
	else return CLOSURE_ADD5(f,a1,a2,a3,a4,a5);
    }
    else return aux_CLOSURE_APPLY5(f,a1,a2,a3,a4,a5);
}

void DO_EVAL(VAL x) {
    VAL result;
//    VAL x = (VAL)(*xin);
    fun* fn;
    thunk* th;
    int excess;

    switch(GETTY(x)) {
    case CON:
    case INT:
    case FLOAT:
    case STRING:
    case UNIT:
	return; // Already evaluated
    case FUN:
	// If the number of arguments is right, run it.
	fn = (fun*)(x->info);
	excess = (fn->arg_end - fn->args) - fn->arity;
	if (excess == 0) {
	    result = fn->fn(fn->args);
	    // If the result is still a function, better eval again to make
	    // more progress
	    if (GETTY(result)==FUN || GETTY(result)==THUNK) {
		DO_EVAL(result);
	    }
	    UPDATE(x,result);
	}
	// If there are too many arguments, run it with the right number
	// then apply the remaining arguments to the resulting closure
	else if (excess > 0) {
	    result = fn->fn(fn->args);
	    result = CLOSURE_APPLY(result, excess, fn->args + fn->arity);
	    DO_EVAL(result);
	    UPDATE(x,result);
	}
	break;
    case THUNK:
	th = (thunk*)(x->info);
	// Evaluate inner thunk, which should give us a function
	DO_EVAL((VAL)(th->fn));
	// Apply this thunk's arguments to it
	CLOSURE_APPLY((VAL)th->fn, th->numargs, th->args);
	// And off we go again...
	DO_EVAL((VAL)(th->fn));
	UPDATE(x,((VAL)(th->fn)));
	break;
    default:
	assert(0); // Can't happen
    }
}

/*
void* DO_PROJECT(VAL x, int arg)
{
    assert(x->ty == CON);
    con* cn = (con*)x->info;
    return cn->args[arg];
}
*/

void* MKINT(int x)
{
    VAL c = MKCLOSURE;
    SETTY(c, INT);
    c->info = (void*)x;
    return c;
}

void* NEWBIGINT(char* intstr)
{
    mpz_t* bigint;
    VAL c = EMALLOC(sizeof(Closure)+sizeof(mpz_t));
    bigint = (mpz_t*)(c+1);
    mpz_init(*bigint);
    mpz_set_str(*bigint, intstr, 10);

    SETTY(c, BIGINT);
    c->info = (void*)bigint;
    return c;
}

void* MKBIGINT(mpz_t* big)
{
    mpz_t* bigint;
    VAL c = EMALLOC(sizeof(Closure)+sizeof(mpz_t));
    bigint = (mpz_t*)(c+1);
    mpz_init(*bigint);
    mpz_set(*bigint, *big);

    SETTY(c, BIGINT);
    c->info = (void*)bigint;
    return c;
}

int GETINT(void* x)
{
    return (int)(((VAL)x)->info);
}

mpz_t* GETBIGINT(void* x)
{
    return ((mpz_t*)(((VAL)x)->info));
}


void* MKSTR(char* x)
{
    VAL c = MKCLOSURE;
    SETTY(c, STRING);
    c->info = (void*)(EMALLOC(strlen(x)*sizeof(char)+1));
    strcpy(c->info,x);
    return c;
}

char* GETSTR(void* x)
{
    return (char*)(((VAL)x)->info);
}

void ERROR(char* msg)
{
    printf("*** error : %s ***\n",msg);
    exit(1);
}

void* MKFREE(int x)
{
    VAL c = MKCLOSURE;
    SETTY(c, FREEVAR);
    c->info = (void*)x;
    return c;
}

void init_evm()
{
    one = MKINT(1);
}
