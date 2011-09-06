/* Copyright (c) 2010 Paul Meier
* 
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
* 
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
* 
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
* THE SOFTWARE.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "parser.h"
#include "constants.h"
#include "gaddag.h"
#include "constants.h"


// A liberal guess that no word is > 30 characters.
#define LONGEST_LINE_LENGTH 30

void format_string_for_gaddag(char*);
char* split_into_representations(const char*);
void reverse_in_place(char*, char*);

/******************************************************************************
 *                               PUBLIC INTERFACE                             *
 ******************************************************************************/ 



gaddag*
parse_gaddag(const char* filename)
{
	// Instantiate the root GADDAG
	gaddag* root_gaddag = create_gaddag();
	// Open the input file handle
	FILE* fp = fopen(filename, "r");
	if (fp == 0)
	{
		fprintf(stderr, "Filename %s is invalid. Please try again.\n", filename);
	}
	char* linebuf = (char*) malloc(LONGEST_LINE_LENGTH * sizeof(char));
	memset(linebuf, 0, LONGEST_LINE_LENGTH);

	// For each line
	while (fscanf(fp, "%s", linebuf) != EOF)
	{
		// prepare it (uppercase + strip), 
		format_string_for_gaddag(linebuf);
		// split into representations.
		char* representations = split_into_representations(linebuf);
		// add to the Gaddag
		add_to_gaddag(root_gaddag, linebuf, representations);

		memset(linebuf, 0, LONGEST_LINE_LENGTH);
		free(representations);
	}

	free(linebuf);
	fclose(fp);
	return root_gaddag;
}


/******************************************************************************
 *                                    HELPERS                                 *
 ******************************************************************************/ 


// Modifies the string in-place to be suitable for the GADDAG
// (uppercase, no whitespace anywhere)
void
format_string_for_gaddag(char* str)
{
	if (DEBUG) printf("---------\ninput string is %s\n", str);

	char* ref = str;
	char curr = *str;
	while (curr != '\0' && curr != '\n' && curr != '\r')
	{
		if (curr >= 'a' && curr <= 'z')
		{
			*ref = curr - ('a' - 'A');
		}
		else if ( !(curr >= 'A' && curr <= 'Z') )
		{
			fprintf(stderr, "Non-standard character in string %s\n", str);
			exit(EXIT_FAILURE);
		}
		++ref;
		curr = *ref;
	}

	if (DEBUG) printf("output string is %s\n", str);
}


// Given a string, returns a fresh array of all of it's textual 
// representations (e.g. PAUL -> P&AUL, AP&UL, UAP&L, LUAP&).
// These are malloc-ed here, but freed later by the caller.
char*
split_into_representations(const char* formatted)
{
	// Get the string's length N.
	const unsigned length = (unsigned) strlen(formatted);
	const unsigned rep_length = length + 1; // separator character '&'
	const unsigned num_cells = length * rep_length + 1;

	// malloc N * N + 1 memory, memset.
	char* representations = (char*) malloc(num_cells * sizeof(char));
	memset(representations, 0, num_cells);

	// Fill the slots with the representations.
	// First, place ampersands between progressing characters.
	unsigned row, col, str_index;
	for (row = 0;  row < length; ++row)
	{
		for (col = 0, str_index = 0; col < rep_length + 1; ++col, ++str_index)
		{
			unsigned index = (row * rep_length) + col;
			representations[index] = formatted[str_index];

			if ((row + 1) == col)
			{
				++col;
				representations[index] = SEPARATOR;
				representations[index + 1] = formatted[str_index];
			}
		}
	}

	// reverse the parts before the &
	for (row = 0; row < length; ++row)
	{
		char* begin = representations + (row * rep_length);
		char* end;

		unsigned end_index = 0;
		while ( *(begin + end_index) != '&') ++end_index;
		end = begin + (end_index - 1);

		reverse_in_place(begin, end);
	}

	if (DEBUG)
	{
		printf("Representations for %s:\n", formatted);
		for (row = 0;  row < length; ++row)
		{
			printf("  ");
			for (col = 0; col < rep_length; ++col)
			{
				unsigned index = (row * rep_length) + col;
				printf("%c", representations[index]);
			}
			printf("\n");
		}
	}

	return representations;
}


// Reverses characters between beginning and end, until the two converge.  e.g. 
// ABST&INENCE would have 'begin' at 'A" and 'end' at 'T'
void
reverse_in_place(char* begin, char* end)
{
	char tmp;
	while ( begin < end )
	{
		tmp = *begin;

		*begin = *end;
		*end = tmp;

		++begin;
		--end;
	}
	return;
}
