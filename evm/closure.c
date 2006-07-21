#include "closure.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

VAL CLOSURE(func x, int arity, int args, void** block)
{
    VAL c = MKCLOSURE;
    fun* fn = MKFUN;
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

    c->ty = FUN;
    c->info = (void*)fn;
    return c;
}

VAL CONSTRUCTOR(int tag, int arity, void** block)
{
    VAL c = MKCLOSURE;
    con* cn = MKCON;
    cn->tag = tag;
    if (arity==0) {
	cn->args = 0;
    } else {
	cn->args = MKARGS(arity);
	memcpy((void*)(cn->args), (void*)block, arity*sizeof(VAL));
    }
    c->ty = CON;
    c->info = (void*)cn;
    return c;
}

VAL CONSTRUCTOR1(int tag, VAL a1)
{
    VAL c = MKCLOSURE;
    con* cn = MKCON;
    cn->tag = tag;
    cn->args = MKARGS(1);
    cn->args[0] = a1;
    c->ty = CON;
    c->info = (void*)cn;
    return c;
}

VAL CONSTRUCTOR2(int tag, VAL a1, VAL a2)
{
    VAL c = MKCLOSURE;
    con* cn = MKCON;
    cn->tag = tag;
    cn->args = MKARGS(2);
    cn->args[0] = a1;
    cn->args[1] = a2;
    c->ty = CON;
    c->info = (void*)cn;
    return c;
}

VAL CONSTRUCTOR3(int tag, VAL a1, VAL a2, VAL a3)
{
    VAL c = MKCLOSURE;
    con* cn = MKCON;
    cn->tag = tag;
    cn->args = MKARGS(3);
    cn->args[0] = a1;
    cn->args[1] = a2;
    cn->args[2] = a3;
    c->ty = CON;
    c->info = (void*)cn;
    return c;
}

VAL CONSTRUCTOR4(int tag, VAL a1, VAL a2, VAL a3, VAL a4)
{
    VAL c = MKCLOSURE;
    con* cn = MKCON;
    cn->tag = tag;
    cn->args = MKARGS(2);
    cn->args[0] = a1;
    cn->args[1] = a2;
    cn->args[2] = a3;
    cn->args[3] = a4;
    c->ty = CON;
    c->info = (void*)cn;
    return c;
}

VAL CONSTRUCTOR5(int tag, VAL a1, VAL a2, VAL a3, VAL a4, VAL a5)
{
    VAL c = MKCLOSURE;
    con* cn = MKCON;
    cn->tag = tag;
    cn->args = MKARGS(5);
    cn->args[0] = a1;
    cn->args[1] = a2;
    cn->args[2] = a3;
    cn->args[3] = a4;
    cn->args[4] = a5;
    c->ty = CON;
    c->info = (void*)cn;
    return c;
}

// Argh, this needs to make a copy
VAL CLOSURE_ADDN(VAL xin, int args, void** block)
{
    assert(xin->ty == FUN);

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

VAL CLOSURE_ADD1(VAL xin, VAL a1)
{
    assert(xin->ty == FUN);

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

VAL CLOSURE_ADD2(VAL xin, VAL a1, VAL a2)
{
    assert(xin->ty == FUN);

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

VAL CLOSURE_ADD3(VAL xin, VAL a1, VAL a2, VAL a3)
{
    assert(xin->ty == FUN);

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

VAL CLOSURE_ADD4(VAL xin, VAL a1, VAL a2, VAL a3, VAL a4)
{
    assert(xin->ty == FUN);

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

VAL CLOSURE_ADD5(VAL xin, VAL a1, VAL a2, VAL a3, VAL a4, VAL a5)
{
    assert(xin->ty == FUN);

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

VAL CLOSURE_APPLY(VAL f, int args, void** block)
{
    VAL c = MKCLOSURE;
    thunk* fn = MKTHUNK;
    if (f->ty==FUN) {
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

    c->ty = THUNK;
    c->info = (void*)fn;
    return c;
}

VAL aux_CLOSURE_APPLY1(VAL f, VAL a1)
{
    VAL c = MKCLOSURE;
    thunk* fn = MKTHUNK;
    if (f->ty==FUN) {
	return CLOSURE_ADD1(f,a1);
    }

    fn->fn = (void*)f;
    fn->numargs = 1;
    fn->args = MKARGS(1);
    fn->args[0] = a1;

    c->ty = THUNK;
    c->info = (void*)fn;
    return c;
}

VAL aux_CLOSURE_APPLY2(VAL f, VAL a1, VAL a2)
{
    VAL c = MKCLOSURE;
    thunk* fn = MKTHUNK;
    if (f->ty==FUN) {
	return CLOSURE_ADD2(f,a1,a2);
    }

    fn->fn = (void*)f;
    fn->numargs = 2;
    fn->args = MKARGS(2);
    fn->args[0] = a1;
    fn->args[1] = a2;

    c->ty = THUNK;
    c->info = (void*)fn;
    return c;
}

VAL aux_CLOSURE_APPLY3(VAL f, VAL a1, VAL a2, VAL a3)
{
    VAL c = MKCLOSURE;
    thunk* fn = MKTHUNK;
    if (f->ty==FUN) {
	return CLOSURE_ADD3(f,a1,a2,a3);
    }

    fn->fn = (void*)f;
    fn->numargs = 2;
    fn->args = MKARGS(2);
    fn->args[0] = a1;
    fn->args[1] = a2;
    fn->args[2] = a3;

    c->ty = THUNK;
    c->info = (void*)fn;
    return c;
}

VAL aux_CLOSURE_APPLY4(VAL f, VAL a1, VAL a2, VAL a3, VAL a4)
{
    VAL c = MKCLOSURE;
    thunk* fn = MKTHUNK;
    if (f->ty==FUN) {
	return CLOSURE_ADD4(f,a1,a2,a3,a4);
    }

    fn->fn = (void*)f;
    fn->numargs = 2;
    fn->args = MKARGS(2);
    fn->args[0] = a1;
    fn->args[1] = a2;
    fn->args[2] = a3;
    fn->args[3] = a4;

    c->ty = THUNK;
    c->info = (void*)fn;
    return c;
}

VAL aux_CLOSURE_APPLY5(VAL f, VAL a1, VAL a2, VAL a3, VAL a4, VAL a5)
{
    VAL c = MKCLOSURE;
    thunk* fn = MKTHUNK;
    if (f->ty==FUN) {
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

    c->ty = THUNK;
    c->info = (void*)fn;
    return c;
}

VAL CLOSURE_APPLY1(VAL f, VAL a1)
{
    if (f->ty==FUN) {
	fun* finf = (fun*)(f->info);
	int got = finf->arg_end-finf->args;
	if (finf->arity == (got+1)) {
	    void** block = MKARGS(finf->arity);
	    memcpy(block, finf->args, got*sizeof(VAL));
	    block[got] = a1;
	    return (VAL)(finf->fn(block));
	}
	else return CLOSURE_ADD1(f,a1);
    }
    else return aux_CLOSURE_APPLY1(f,a1);
}

VAL CLOSURE_APPLY2(VAL f, VAL a1, VAL a2)
{
    if (f->ty==FUN) {
	fun* finf = (fun*)(f->info);
	int got = finf->arg_end-finf->args;
	if (finf->arity == (got+2)) {
	    void** block = MKARGS(finf->arity);
	    memcpy(block, finf->args, got*sizeof(VAL));
	    block[got] = a1;
	    block[got+1] = a2;
	    return (VAL)(finf->fn(block));
	}
	else return CLOSURE_ADD2(f,a1,a2);
    }
    else return aux_CLOSURE_APPLY2(f,a1,a2);
}

VAL CLOSURE_APPLY3(VAL f, VAL a1, VAL a2, VAL a3)
{
    if (f->ty==FUN) {
	fun* finf = (fun*)(f->info);
	int got = finf->arg_end-finf->args;
	if (finf->arity == (got+3)) {
	    void** block = MKARGS(finf->arity);
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

VAL CLOSURE_APPLY4(VAL f, VAL a1, VAL a2, VAL a3, VAL a4)
{
    if (f->ty==FUN) {
	fun* finf = (fun*)(f->info);
	int got = finf->arg_end-finf->args;
	if (finf->arity == (got+4)) {
	    void** block = MKARGS(finf->arity);
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

VAL CLOSURE_APPLY5(VAL f, VAL a1, VAL a2, VAL a3, VAL a4, VAL a5)
{
    if (f->ty==FUN) {
	fun* finf = (fun*)(f->info);
	int got = finf->arg_end-finf->args;
	if (finf->arity == (got+5)) {
	    void** block = MKARGS(finf->arity);
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

void EVAL(VAL x) {
    VAL result;
//    VAL x = (VAL)(*xin);
    fun* fn;
    thunk* th;
    int excess;

    switch(x->ty) {
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
	    if (result->ty==FUN || result->ty==THUNK) {
		EVAL(result);
	    }
	    UPDATE(x,result);
	}
	// If there are too many arguments, run it with the right number
	// then apply the remaining arguments to the resulting closure
	else if (excess > 0) {
	    result = fn->fn(fn->args);
	    result = CLOSURE_APPLY(result, excess, fn->args + fn->arity);
	    EVAL(result);
	    UPDATE(x,result);
	}
	break;
    case THUNK:
	th = (thunk*)(x->info);
	// Evaluate inner thunk, which should give us a function
	EVAL((VAL)(th->fn));
	// Add this thunk's arguments to it
	CLOSURE_APPLY((VAL)th->fn, th->numargs, th->args);
	// And off we go again...
	EVAL((VAL)(th->fn));
	UPDATE(x,((VAL)(th->fn)));
	break;
    default:
	assert(0); // Can't happen
    }
}

void* PROJECT(VAL x, int arg)
{
    assert(x->ty == CON);
    con* cn = (con*)x->info;
    return cn->args[arg];
}

void* MKINT(int x)
{
    VAL c = MKCLOSURE;
    c->ty = INT;
    c->info = (void*)x;
    return c;
}

int GETINT(void* x)
{
    return (int)(((VAL)x)->info);
}

void* MKSTR(char* x)
{
    VAL c = MKCLOSURE;
    c->ty = STRING;
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
    c->ty = FREEVAR;
    c->info = (void*)x;
    return c;
}

void printInt(int x) { printf("%d\n",x); }
void putStr(char* s) { printf("%s",s); }

int readInt() {
    return atoi(readStr());
}

char* readStr() {
    char* buf = EMALLOC(512); // yeah, right...
    fgets(buf,512,stdin);
    char *loc = strchr(buf,'\n');
    *loc = '\0';
    return buf;       
}

int strToInt(char* str)
{
    return atoi(str);
}

char* intToStr(int x)
{
    char* buf = EMALLOC(16);
    sprintf(buf,"%d",x);
    return buf;
}

char* append(char* x, char* y) {
    char* buf = EMALLOC((strlen(x)+strlen(y))*sizeof(char));
    strcpy(buf,x);
    strcat(buf,y);
    return buf;
}
