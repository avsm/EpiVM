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

VAL CLOSURE_ADDN(VAL x, int args, void** block)
{
    assert(x->ty == FUN);
    fun* fn = (fun*)(x->info);
    int diff = fn->arg_end - fn->args;

    fn->args = MOREARGS(fn->args, args);
    fn->arg_end = fn->args + diff;

    memcpy((void*)(fn->arg_end), (void*)block, args*sizeof(VAL));
    fn->arg_end += args;
    return x;
}

void EVAL(void** xin) {
    VAL result;
    VAL x = (VAL)(*xin);
    fun* fn;
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
	    UPDATE(xin,result);
	}
	// If there are too many arguments, run it with the right number
	// then apply the remaining arguments to the resulting closure
	else if (excess > 0) {
	    result = fn->fn(fn->args);
	    CLOSURE_ADDN(result, excess, fn->args + fn->arity);
	    UPDATE(xin,result);
	    EVAL(xin);
	}
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
