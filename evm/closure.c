#include "closure.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>

VAL one;
VAL* zcon;

void dumpClosureA(Closure* c, int rec);

void dumpCon(con* c, int rec) {
    int x,arity;
    if (!rec) { printf("TAG(%d)", c->tag & 65535); }

    arity = c->tag >> 16;
    if (arity>0 && !rec) { printf(": "); }

    for(x=0; x<arity; ++x) {
	dumpClosureA(c->args[x], rec);
	if (x!=(arity-1)) { printf(", "); }
    }
}

void dumpRecord(Closure* r) {
    dumpClosureA(r, 1);
}

void dumpClosureA(Closure* c, int rec) {
    switch(GETTY(c)) {
    case FUN:
	printf("FUN[");
	break;
    case THUNK:
	printf("THUNK[");
	break;
    case CON:
	if (!rec) { printf("CON["); } else { printf("["); }
	dumpCon((con*)c->info, rec);
	break;
    case INT:
	if (!rec) { printf("INT[%d", ((int)c)>>1); } else { printf("[%d", ((int)c)>>1); }
	break;
    case BIGINT:
	printf("BIGINT[");
	break;
    case FLOAT:
	printf("FLOAT[");
	break;
    case BIGFLOAT:
	printf("BIGFLOAT[");
	break;
    case STRING:
	printf("STRING[");
	break;
    case UNIT:
	printf("UNIT[");
	break;
    case PTR:
	printf("PTR[");
	break;
    case FREEVAR:
	printf("FREEVAR[");
	break;
    default:
	printf("[%d,%d", GETTY(c), (int)c->info);
    }
    printf("]");
}

void dumpClosure(Closure* c) {
    dumpClosureA(c,0);
    printf("\n");
}

void assertConR(Closure* c) 
{
    if (c==NULL) { printf("Null constructor\n"); assert(0); }
    if (!ISCON(c)) { dumpClosure(c); assert(0); }
}

void assertInt(Closure* c) 
{
    if (!ISINT(c)) { dumpClosure(c); assert(0); }
}

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

inline VAL CONSTRUCTORn(int tag, int arity, void** block)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(con)); // MKCLOSURE;
    con* cn = (con*)(c+1);
    cn->tag = tag + (arity << 16);
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
    VAL c = EMALLOC(sizeof(Closure)+sizeof(con)+sizeof(VAL)); // MKCLOSURE;
    con* cn = (con*)(c+1);
    cn->tag = tag + (1 << 16);
    cn->args = (void*)c+sizeof(Closure)+sizeof(con); // MKARGS(1);
    cn->args[0] = a1;
    SETTY(c,CON);
    c->info = (void*)cn;
    return c;
}

inline VAL CONSTRUCTOR2(int tag, VAL a1, VAL a2)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(con)+2*sizeof(VAL)); // MKCLOSURE;
    con* cn = (con*)(c+1);
    cn->tag = tag + (2 << 16);
    cn->args = (void*)c+sizeof(Closure)+sizeof(con); //MKARGS(2);
    cn->args[0] = a1;
    cn->args[1] = a2;
    SETTY(c,CON);
    c->info = (void*)cn;
    return c;
}

inline VAL CONSTRUCTOR3(int tag, VAL a1, VAL a2, VAL a3)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(con)+3*sizeof(VAL)); // MKCLOSURE;
    con* cn = (con*)(c+1);
    cn->tag = tag + (3 << 16);
    cn->args = (void*)c+sizeof(Closure)+sizeof(con); //MKARGS(3);
    cn->args[0] = a1;
    cn->args[1] = a2;
    cn->args[2] = a3;
    SETTY(c,CON);
    c->info = (void*)cn;
    return c;
}

inline VAL CONSTRUCTOR4(int tag, VAL a1, VAL a2, VAL a3, VAL a4)
{
    VAL c = EMALLOC(sizeof(Closure)+sizeof(con)+4*sizeof(VAL)); // MKCLOSURE;
    con* cn = (con*)(c+1);
    cn->tag = tag + (4 << 16);
    cn->args = (void*)c+sizeof(Closure)+sizeof(con); //MKARGS(2);
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
    VAL c = EMALLOC(sizeof(Closure)+sizeof(con)+5*sizeof(VAL)); // MKCLOSURE;
    con* cn = (con*)(c+1);
    cn->tag = tag + (5 << 16);
    cn->args = (void*)c+sizeof(Closure)+sizeof(con); //MKARGS(5);
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

    fn->args = MOREARGS(fn->args, args + diff);
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

    fn->args = MOREARGS(fn->args, diff + 1);
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

    fn->args = MOREARGS(fn->args, diff + 2);
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

    fn->args = MOREARGS(fn->args, diff + 3);
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

    fn->args = MOREARGS(fn->args, diff + 4);
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

    fn->args = MOREARGS(fn->args, diff + 5);
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
	return NULL; //CLOSURE_ADD2(f,a1,a2);
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
    fn->args = MKARGS(3);
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
    fn->args = MKARGS(4);
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
    fn->args = MKARGS(5);
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
	    void* block[got+1];
	    memcpy(block, finf->args, got*sizeof(VAL));
	    block[got] = a1;
	    return (VAL)(finf->fn(block));
	}
	else return CLOSURE_ADD1(f,a1);
    }
    else return aux_CLOSURE_APPLY1(f,a1);
}

void* block[1024]; // Yes. I know. Better check below that this is big enough.

inline VAL CLOSURE_APPLY2(VAL f, VAL a1, VAL a2)
{
    int i;
    if (ISFUN(f)) {
	fun* finf = (fun*)(f->info);
	int got = finf->arg_end-finf->args;
	if (finf->arity == (got+2)) {
//	    memcpy(block, finf->args, got*sizeof(VAL));
	    for(i=0; i<got; ++i) {
		block[i] = finf->args[i];
	    }
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
	    void* block[got+3];
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
	    void* block[got+4];
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
	    void* block[got+5];
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

VAL DO_EVAL(VAL x, int update) {
// dummy value we'll never inspect, leave it alone.
    if (x==NULL) return x; 

    VAL result;
//    VAL x = (VAL)(*xin);
    fun* fn;
    thunk* th;
    int excess;

//    dumpClosure(x);

    switch(GETTY(x)) {
    case CON:
    case INT:
    case FLOAT:
    case STRING:
    case PTR:
    case UNIT:
	return x; // Already evaluated
    case FUN:
	// If the number of arguments is right, run it.
	fn = (fun*)(x->info);
	excess = (fn->arg_end - fn->args) - fn->arity;
	if (excess == 0) {
	    result = fn->fn(fn->args);
	    // If the result is still a function, better eval again to make
	    // more progress.
	    // It could reasonably be null though, so be careful. It's null
	    // if it was a foreign/io call in particular.
	    if (result) {
		if (GETTY(result)==FUN || GETTY(result)==THUNK) {
		    result=DO_EVAL(result, update);
		}
/*		if (ISINT(result)) {
		    printf("Updating with %d\n", x);
		} else {
		    printf("Updating %d %d with %d\n", x, GETTY(x), result);
		    }*/
		if (update) { UPDATE(x,result); } else { return result; }
	    }
	    else {
		if (update) { SETTY(x, INT); x->info=(void*)42; } else { return NULL; }
	    }
	}
	// If there are too many arguments, run it with the right number
	// then apply the remaining arguments to the resulting closure
	else if (excess > 0) {
	    result = fn->fn(fn->args);
	    result = CLOSURE_APPLY(result, excess, fn->args + fn->arity);
	    result = DO_EVAL(result, update);
	    if (update) { UPDATE(x,result); } else { return result; }
	    return x;
	}
	break;
    case THUNK:
	th = (thunk*)(x->info);
	// Evaluate inner thunk, which should give us a function
	VAL nextfn = DO_EVAL((VAL)(th->fn), update);
	// Apply this thunk's arguments to it
	CLOSURE_APPLY((VAL)nextfn, th->numargs, th->args);
	// And off we go again...
	nextfn = DO_EVAL(nextfn, update);
	if (update) { UPDATE(x, nextfn); } else { return nextfn; }
	return x;
	break;
    default:
	assert(0); // Can't happen
    }
    return x;
}

/*
void* DO_PROJECT(VAL x, int arg)
{
    assert(x->ty == CON);
    con* cn = (con*)x->info;
    return cn->args[arg];
}
*/

 /*void* MKINT(int x)
{
    return (void*)((x<<1)+1);
//    VAL c = MKCLOSURE;
//    SETTY(c, INT);
//    c->info = (void*)x;
//    return c;
}*/

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

/*
int GETINT(void* x)
{
    return ((int)x)>>1;
}
*/

mpz_t* GETBIGINT(void* x)
{
    return ((mpz_t*)(((VAL)x)->info));
}


void* MKSTR(char* x)
{
    VAL c = EMALLOC(sizeof(Closure)+strlen(x)+sizeof(char)+1); //MKCLOSURE;
    SETTY(c, STRING);
    c->info = ((void*)c)+sizeof(Closure);// (void*)(EMALLOC(strlen(x)*sizeof(char)+1));
    strcpy(c->info,x);
    return c;
}

void* MKPTR(void* x)
{
    VAL c = MKCLOSURE;
    SETTY(c, PTR);
    c->info = x;
    return c;
}

/* void* GETPTR(void* x) */
/* { */
/*     return (void*)(((VAL)x)->info); */
/* } */

void ERROR(char* msg)
{
    printf("*** error : %s ***\n",msg);
    assert(0);
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
    int i;
    one = MKINT(1);
    zcon = EMALLOC(sizeof(Closure)*255);
    for(i=0;i<255;++i) {
	zcon[i] = CONSTRUCTORn(i,0,0);
    }
}
