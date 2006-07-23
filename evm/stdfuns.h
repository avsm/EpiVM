#ifndef _STDFUNS_H
#define _STDFUNS_H

#include <gc/gc.h>
#include <gmp.h>
#include <stdio.h>

// Some basic communication with the outside world

void putStr(char* str);
void printInt(int x);
void printBigInt(mpz_t x);

int readInt();
char* readStr();

int strToInt(char* str);
char* intToStr(int x);

int strToBigInt(char* str);
char* bigIntToStr(mpz_t x);

// Big integer arithmetic

mpz_t* addBigInt(mpz_t x, mpz_t y);
mpz_t* subBigInt(mpz_t x, mpz_t y);
mpz_t* mulBigInt(mpz_t x, mpz_t y);
mpz_t* divBigInt(mpz_t x, mpz_t y);

int eqBigInt(mpz_t x, mpz_t y);

#endif

