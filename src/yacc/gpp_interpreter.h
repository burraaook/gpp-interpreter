#ifndef gpp_interpreter_h
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CAP 75

/*
    *  Symbol Table
    *  ------------
    *  key: identifier
    *  val: fraction
    *  next: pointer to next element
*/
typedef struct symbol_table {
    char *key;
    char *val;
    struct symbol_table *next;
}symbol_table;

/*
    *  Function Table
    *  --------------
    *  key: function name
    *  val: function body
    *  next: pointer to next element
*/
typedef struct function_table {
    char *key;
    char *val;
    struct function_table *next;
}function_table;

/*symbol table implementations*/
char* set_id(symbol_table **table, char *id, char *val);
char* get_id(symbol_table **table, char *id);
char* add_frac(char *fval1, char *fval2);
char* sub_frac(char *fval1, char *fval2);
char* mul_frac(char *fval1, char *fval2);
void get_parts(char *fval, int *nom, int *denom);
void simplify(int *nom, int *denom);
int find_gcd(int num1, int num2);
char* get_frac(int nom, int denom);
void get_parts(char *frac, int *nom, int *denom);
char* add_helper(int nom1, int denom1, int nom2, int denom2);
char* sub_helper(int nom1, int denom1, int nom2, int denom2);
char* mul_helper(int nom1, int denom1, int nom2, int denom2);
int frac_eq(char* frac1, char* frac2);
int frac_gt(char* frac1, char* frac2);

/*function table implementations*/
char* set_func(function_table **table, char *id, char *val);
char* get_func(function_table **table, char *id);

char* set_id(symbol_table **table, char *id, char* frac) {
    
    int i = 0;
    symbol_table *temp = *table;
    symbol_table *temp2;

    /*if empty, create*/
    if (*table == NULL) {
        *table = malloc(sizeof(symbol_table));
        (*table)->key = malloc(sizeof(strlen(id)*sizeof(char) + 1));
        strcpy((*table)->key, id);
        (*table)->val = malloc(sizeof(strlen(frac)*sizeof(char) + 1));
        strcpy((*table)->val, frac);
        (*table)->next = NULL;
        return (*table)->val;
    }
    
    /*if exists set value*/
    while(temp != NULL) {
        if(strcmp(temp->key, id) == 0) {
            temp->val = malloc(sizeof(char)*strlen(frac) + 1);
            strcpy(temp->val, frac);
            return temp->val;
        }
        temp = temp->next;
    }

    /*insert at head*/
    temp2 = malloc(sizeof(symbol_table));
    temp2->next = *table;
    temp2->key = malloc(sizeof(strlen(id)*sizeof(char) + 1));
    strcpy(temp2->key, id);
    temp2->val = malloc(sizeof(strlen(frac)*sizeof(char) + 1));
    strcpy(temp2->val, frac);
    *table = temp2;
    return (*table)->val;
}

/*returns NULL if not found*/
char* get_id(symbol_table **table, char *id) {
    
    symbol_table *temp = *table;
    while(temp != NULL) {
        if(strcmp(temp->key, id) == 0) {
            return temp->val;
        }
        temp = temp->next;
    }
    return NULL;
}

/* add two fractions */
char* add_frac(char *fval1, char *fval2) {
    int nom1, denom1, nom2, denom2, res_nom, res_denom;
    get_parts(fval1, &nom1, &denom1);
    get_parts(fval2, &nom2, &denom2);

    return add_helper(nom1, denom1, nom2, denom2);
}

char* add_helper(int nom1, int denom1, int nom2, int denom2) {
    int res_nom, res_denom;

    res_nom = nom1*denom2 + nom2*denom1;
    res_denom = denom1*denom2;

    simplify(&res_nom, &res_denom);
    return get_frac(res_nom, res_denom);
}

/* subtract two fractions */
char* sub_frac(char *fval1, char *fval2) {
    int nom1, denom1, nom2, denom2, res_nom, res_denom;

    get_parts(fval1, &nom1, &denom1);
    get_parts(fval2, &nom2, &denom2);

    return sub_helper(nom1, denom1, nom2, denom2);
}

char* sub_helper(int nom1, int denom1, int nom2, int denom2) {
    int res_nom, res_denom;

    res_nom = nom1*denom2 - nom2*denom1;
    res_denom = denom1*denom2;

    simplify(&res_nom, &res_denom);
    return get_frac(res_nom, res_denom);
}

/* multiply two fractions */
char* mul_frac(char *fval1, char *fval2) {
    int nom1, denom1, nom2, denom2, res_nom, res_denom;

    get_parts(fval1, &nom1, &denom1);
    get_parts(fval2, &nom2, &denom2);

    return mul_helper(nom1, denom1, nom2, denom2);
}

char* mul_helper(int nom1, int denom1, int nom2, int denom2) {
    int res_nom, res_denom;

    res_nom = nom1*nom2;
    res_denom = denom1*denom2;

    simplify(&res_nom, &res_denom);
    return get_frac(res_nom, res_denom);
}

/* simplifies given fraction */
void simplify(int *nom, int *denom) {
    int gcd = find_gcd(*nom, *denom);
    *nom = (*nom) / gcd;
    *denom = (*denom) / gcd;
}

/* find greatest common divisor */
int find_gcd(int num1, int num2) {

    if (num2 == 0) {
        return num1;
    }
    return find_gcd(num2, num1 % num2);
}

char* get_frac(int nom, int denom) {
    char *frac = malloc(sizeof(char)*(CAP));
    sprintf(frac, "%df%d", nom, denom);
    return frac;
}

/* returns 1 if equal, 0 otherwise */
void get_parts(char *frac, int *nom, int *denom) {

    char temp[CAP];
    char temp2[CAP];
    char *token;
    
    strcpy(temp, frac);
    strcpy(temp2, frac);
    
    token = strtok(temp,"f");
    *nom = atoi(temp);
    *denom = atoi(&temp2[strlen(temp) + 1]);
}

/* returns 1 if equal, 0 otherwise */
int frac_eq(char* frac1, char* frac2) {
    int nom1, denom1, nom2, denom2;
    get_parts(frac1, &nom1, &denom1);
    get_parts(frac2, &nom2, &denom2);

    return (nom1*denom2 == nom2*denom1);
}
int frac_gt(char* frac1, char* frac2) {
    int nom1, denom1, nom2, denom2;
    get_parts(frac1, &nom1, &denom1);
    get_parts(frac2, &nom2, &denom2);

    return (nom1*denom2 > nom2*denom1);    
}

char* set_func(function_table **table, char *id, char *val) {
    function_table *temp = *table;
    function_table *temp2;
    /*if empty*/
    if(*table == NULL) {
        *table = malloc(sizeof(function_table));
        (*table)->key = malloc(sizeof(strlen(id)*sizeof(char) + 1));
        strcpy((*table)->key, id);
        (*table)->val = malloc(sizeof(strlen(val)*sizeof(char) + 1));
        strcpy((*table)->val, val);
        (*table)->next = NULL;
        return (*table)->val;
    }
    
    /*if exists set value*/
    while(temp != NULL) {
        if(strcmp(temp->key, id) == 0) {
            temp->val = malloc(sizeof(char)*strlen(val) + 1);
            strcpy(temp->val, val);
            return temp->val;
        }
        temp = temp->next;
    }

    /*insert at head*/
    temp2 = malloc(sizeof(function_table));
    temp2->next = *table;
    temp2->key = malloc(sizeof(strlen(id)*sizeof(char) + 1));
    strcpy(temp2->key, id);
    temp2->val = malloc(sizeof(strlen(val)*sizeof(char) + 1));
    strcpy(temp2->val, val);
    *table = temp2;
    return (*table)->val;
}

char* get_func(function_table **table, char *id) {
    function_table *temp = *table;
    while(temp != NULL) {
        if(strcmp(temp->key, id) == 0) {
            return temp->val;
        }
        temp = temp->next;
    }
    return NULL;
}

#endif