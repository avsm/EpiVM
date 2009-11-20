#include "stdfuns.h"
#include "closure.h"
#include <stdlib.h>
#include <gmp.h>
#include <string.h>

void printInt(int x) { printf("%d\n",x); }
void putStr(char* s) { printf("%s",s); }
void printBigInt(mpz_t x) { printf("%s\n",mpz_get_str(NULL,10,x)); }

void epicGC() {
    GC_gcollect();
}

void epicMemInfo() {
    GC_gcollect();
    int heap = GC_get_heap_size();
    int free = GC_get_free_bytes();
    int total = GC_get_total_bytes();

    printf("Heap size %d\n", heap);
    printf("Heap used %d\n", heap-free);
    printf("Total allocations %d\n", total);
}

int readInt() {
    return atoi(readStr());
}

// FIXME: Do this properly!
char* readStr() {
    char *buf = NULL;
    if (buf==NULL) { buf = EMALLOC(sizeof(char)*512); } // yeah, right...
    fgets(buf,512,stdin);
    char *loc = strchr(buf,'\n');
    *loc = '\0';
    return buf;
}

// FIXME: Do this properly!
void* freadStr(void* h) {
    static char bufin[128];
    bufin[0]='\0';

    FILE* f = (FILE*)h;
    fgets(bufin,128,f);
    int len = strlen(bufin);

    VAL c = GC_MALLOC_ATOMIC(sizeof(Closure)+len*sizeof(char)+sizeof(char)+1);
    SETTY(c, STRING);
    c->info = ((void*)(c+1));
    char *buf = (char*)(c->info);
    strcpy(buf,bufin);

    char *loc = strchr(buf,'\n');
    if (loc) *loc = '\0'; else buf[0]='\0';
    return ((void*)c);
}

void fputStr(void* h, char* str) {
    FILE* f = (FILE*)h;
    fputs(str, f);
}

int streq(char* x, char* y) {
    return !(strcmp(x,y));
}

int strlt(char* x, char* y) {
    return strcmp(x,y)<0;
}

int strToInt(char* str)
{
    return strtol(str,NULL,10);
}

char* intToStr(int x)
{
    char* buf = EMALLOC(16);
    sprintf(buf,"%d",x);
    return buf;
}

void* getNative(void * fn) {
    return fn;
}

int strIndex(char* str, int i)
{
    return (int)(str[i]);
}

int strHead(char* str) {
    if (str[0]=='\0') 
	ERROR("Can't take the head of an empty string");
    return (int)(str[0]);
}

char* strTail(char* str) {
    if (str[0]=='\0') 
	ERROR("Can't take the tail of an empty string");
    return str+1; // I'll need to check the GC will understand this...
}

char* strCons(int h, char* str) {
    char* buf = EMALLOC((1+strlen(str))*sizeof(char));
    buf[0]=(char)h;
    strcpy(buf+1, str);
    return buf;
}

char* append(char* x, char* y) {
    char* buf = EMALLOC((strlen(x)+strlen(y))*sizeof(char));
    strcpy(buf,x);
    strcat(buf,y);
    return buf;
}

mpz_t* addBigInt(mpz_t x, mpz_t y) {
    mpz_t* answer = EMALLOC(sizeof(mpz_t));
    mpz_add(*answer, x, y);
    return answer;
}

mpz_t* subBigInt(mpz_t x, mpz_t y) {
    mpz_t* answer = EMALLOC(sizeof(mpz_t));
    mpz_sub(*answer, x, y);
    return answer;
}

mpz_t* mulBigInt(mpz_t x, mpz_t y) {
    mpz_t* answer = EMALLOC(sizeof(mpz_t));
    mpz_mul(*answer, x, y);
    return answer;
}

mpz_t* divBigInt(mpz_t x, mpz_t y) {
    mpz_t* answer = EMALLOC(sizeof(mpz_t));
    mpz_tdiv_q(*answer, x, y);
    return answer;
}

mpz_t* modBigInt(mpz_t x, mpz_t y) {
    mpz_t* answer = EMALLOC(sizeof(mpz_t));
    mpz_tdiv_r(*answer, x, y);
    return answer;
}

int eqBigInt(mpz_t x, mpz_t y) {
    return mpz_cmp(x,y)==0;
}

int ltBigInt(mpz_t x, mpz_t y)
{
    return mpz_cmp(x,y)<0;
}

int gtBigInt(mpz_t x, mpz_t y)
{
    return mpz_cmp(x,y)>0;
}

int leBigInt(mpz_t x, mpz_t y)
{
    return mpz_cmp(x,y)<=0;
}

int geBigInt(mpz_t x, mpz_t y)
{
    return mpz_cmp(x,y)>=0;
}

mpz_t* strToBigInt(char* str)
{
    mpz_t* answer = EMALLOC(sizeof(mpz_t));
    mpz_init(*answer);
    mpz_set_str(*answer, str, 10);
    return answer;
}

char* bigIntToStr(mpz_t x)
{
    char* str = mpz_get_str(NULL,10,x);
    char* buf = EMALLOC(strlen(str)+1);
    strcpy(buf,str);
    free(str);
    return buf;
}

// IORefs
int numrefs = 0;
void** iorefs = NULL;

int newRef() {
    // Increase space for the iorefs
    if (iorefs==NULL) {
	iorefs = (void**)(EMALLOC(sizeof(void*)));
	numrefs=1;
    } else {
	iorefs = (void**)(EREALLOC(iorefs, sizeof(void*)*(numrefs+1)));
	numrefs++;
    }
    return numrefs-1;
}

void* readRef(int r) {
    return iorefs[r];
}

void writeRef(int r, void* val) {
    iorefs[r]=val;
}

// Threads and locks

typedef struct {
    pthread_mutex_t m_id;
} Mutex;

typedef struct {
    pthread_t t_id;
} Thread;

Mutex** ms = NULL;
int mutexes = 0;

int newLock(int sem)
{
    pthread_mutex_t m;

    pthread_mutex_init(&m, NULL);
    Mutex* newm = EMALLOC(sizeof(Mutex));
    newm->m_id = m;

    // Increase space for the mutexes
    if (ms==NULL) {
	ms = (Mutex**)EMALLOC(sizeof(Mutex*));
	mutexes=1;
    } else {
	ms = (Mutex**)(EREALLOC(ms, sizeof(Mutex*)*(mutexes+1)));
	mutexes++;
    }

    ms[mutexes-1] = newm;
    return mutexes-1;
}

void doLock(int lock)
{
    pthread_mutex_lock(&(ms[lock]->m_id));
}

void doUnlock(int lock)
{
    pthread_mutex_unlock(&(ms[lock]->m_id));
}

void* runThread(void* proc) {
    DO_EVAL(proc, 1);
    return NULL;
}

void doFork(void* proc)
{
    pthread_t* t = EMALLOC(sizeof(pthread_t));
//    printf("CREATING THREAD %d\n", t);
//    int r = 
    pthread_create(t, NULL, runThread, proc);
//    printf("THREAD CREATED %d\n", r);
}


// Basic file handling

void* fileOpen(char* name, char* mode) {
    FILE* f = fopen(name, mode);
    return (void*)f;
}

void fileClose(void* h) {
    FILE* f = (FILE*)h;
    fclose(f);
}

int isNull(void* ptr) {
    return ptr==NULL;
}
