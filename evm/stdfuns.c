#include "stdfuns.h"
#include "closure.h"
#include <stdlib.h>
#include <gmp.h>
#include <string.h>

void printInt(int x) { printf("%d\n",x); }
void putStr(char* s) { printf("%s",s); }
void printBigInt(mpz_t x) { printf("%s\n",mpz_get_str(NULL,10,x)); }

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

int strIndex(char* str, int i)
{
    return (int)(str[i]);
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
    mpz_cdiv_q(*answer, x, y);
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

