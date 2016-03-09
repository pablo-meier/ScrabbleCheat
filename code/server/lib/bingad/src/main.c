#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "gaddag.h"
#include "parser.h"
#include "to_binary.h"
#include "constants.h"


#define MAX_FILE_NAME_SIZE 100

// Declarations....
void outfile_name(const char*, char*);


/*
 * Usage: ./gaddag_parser file [file2 ...]
 *
 * Note that each file MUST be of the form <name>.txt, and will be output as
 * <name>.dict
 */
int
main(int argc, char* argv[])
{
	if (argc < 2) {
		fprintf(stderr, "Usage: gaddag_parser file [file2 ...]\n\n");
		fprintf(stderr, "Must supply at least one argument.\n");
		exit(EXIT_FAILURE);
	}

	int i;
	for(i = 1; i < argc; ++i) 
	{
		char* filename = argv[i];
		char outfile[MAX_FILE_NAME_SIZE];
		outfile_name(filename, outfile);

		gaddag* parsed = parse_gaddag(filename);

		gaddag_to_binary(parsed, outfile);
		delete_gaddag(parsed);
	}

	exit(0);
}



/*
 * Given an input name, produces the appropriate output name.
 */
void
outfile_name(const char* infile, char* outfile)
{
	size_t length = strlen(infile);

	// we subtract 1 because *.dict is one character greater than *.txt, and we use
	// MAX_FILE_NAME_SIZE as the buffer size.
	if ((length + 2) > MAX_FILE_NAME_SIZE) 
	{
		fprintf(stderr, "Error: max name for a filename is %d, submitted one of length %d\n",
				MAX_FILE_NAME_SIZE - 2, (int) length);
        fprintf(stderr, "Read filename %s\n", outfile);
		exit(EXIT_FAILURE);
	}
	
	strlcpy(outfile, infile, length);

	outfile[length - 3] = 'd';
	outfile[length - 2] = 'i';
	outfile[length - 1] = 'c';
	outfile[length    ] = 't';
	outfile[length + 1] = '\0';
}

