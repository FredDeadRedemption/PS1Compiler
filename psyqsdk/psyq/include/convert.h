/*
 * File:convert.h
 */
/*
 * $PSLibId: Run-time Library Release 4.7$
 */
#ifndef _CONVERT_H
#define _CONVERT_H

#if defined(_LANGUAGE_C_PLUS_PLUS)||defined(__cplusplus)||defined(c_plusplus)
extern "C" {
#endif
extern int atoi(const char *);
extern long atol(const char *);
extern long strtol(const char *,char**, int);
extern unsigned long strtoul(const char *, char **, int);
extern long labs(long);

#if defined(_LANGUAGE_C_PLUS_PLUS)||defined(__cplusplus)||defined(c_plusplus)
}
#endif

#endif
