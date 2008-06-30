#ifndef _STDFUNS_H
#define _STDFUNS_H

#include <gc/gc.h>
#include <gmp.h>
#include <stdio.h>
#include "closure.h"

// Some basic communication with the outside world

void putStr(char* str);
void printInt(int x);
void printBigInt(mpz_t x);

int readInt();
char* readStr();

// IORefs
int newRef();
void* readRef(int r);
void writeRef(int r, void* val);

int strToInt(char* str);
char* intToStr(int x);

mpz_t* strToBigInt(char* str);
char* bigIntToStr(mpz_t x);

// String operations

int strIndex(char* str, int i);
char* append(char* x, char* y);

// Big integer arithmetic

mpz_t* addBigInt(mpz_t x, mpz_t y);
mpz_t* subBigInt(mpz_t x, mpz_t y);
mpz_t* mulBigInt(mpz_t x, mpz_t y);
mpz_t* divBigInt(mpz_t x, mpz_t y);

int eqBigInt(mpz_t x, mpz_t y);
int ltBigInt(mpz_t x, mpz_t y);
int gtBigInt(mpz_t x, mpz_t y);
int leBigInt(mpz_t x, mpz_t y);
int geBigInt(mpz_t x, mpz_t y);

#endif

