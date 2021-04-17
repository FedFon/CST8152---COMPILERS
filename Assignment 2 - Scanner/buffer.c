/*
*  File Name: buffer.c
*   Compiler: MS Visual Studio 2019
*     Author: Federico Fonseca
*     Course: CST8152 - Compilers, Lab Section: 011
*Assignement: 01
*       Date: 2020-06-22
*  Professor: Svillen Ranev
* Purpose:This is a review of and an exercise in C coding style, 
* programming techniques, data types and structures, memory management, 
* and simple file input/output. It will give you a better understanding of the 
* type of internal data structures used by a simple compiler you will be building 
* this semester. This assignment will be also an exercise in “excessively defensive programming”. 
* You are to write functions that should be “overly” protected and should not abruptly terminate 
* or “crash” at run-time due to invalid function parameters, erroneous internal calculations,
* or memory violations.
* Buffer* b_allocate()
* pBuffer b_addc()
* int b_clear()
* void b_free()
* int b_isfull()
* short b_addcoffset()  
* short b_capacity()
* short b_markc()
* int b_mode()
* size_t b_incfactor()
* int b_load()
* int b_isempty()
* char b_getc()
* int b_eob()
* int b_print()
* Buffer* b_compact()
* char b_rflag()
* short b_retract()
* short b_reset()
* short b_getcoffset()
* int b_rewind()
* char* b_location()
*/
#include "buffer.h"
#include <stdlib.h>

/*
* Purpose: This function tries to allocate memory for a BufferDescriptor
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters: init_capacity, inc_factor, o_mode
* Return value: BufferDescriptor
* Algorithm: Uses calloc  to allocate memory size of Buffer. If unsuccessful, returns NULL.
* if init capacity is in between 0 and SHRT_MAX-1 (inclusive), malloc is used to allocate memory
* for a dynamic character array. if init_capacity is 0, memory for character array is allocated
* with a default size of 200 and will set inc_factor to 15 in modes: a, m. 0 in mode f. Pointer returned
* by malloc is assigned to cb_head. if o_mode is f, mode and inc_factor are set to 0.
* if inc_factor is 0 and init_capacity is not 0, mode and inc_factor are set to 0.
* if o_mode is a and inc_factor is in range of 1 to 255 (inclusive), mode is set to 1 and
* inc_factor is set to inc_factor. if o_mode is set to m, and inc_factor is in range of 1 to 100
* (inclusive), mode is set to -1 and inc_factor is set to inc_factor.
* init_capacity is set to capacity of the instance of the BufferDescriptor.
* Sets flags to default value FFF9 hexadecimal.
*/
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode) {
	/*buffer pointer and character array*/
	pBuffer buff;
	char* buffArr;

	/*Using calloc to create a buffer pointer on heap*/
	buff = (pBuffer)calloc(1, sizeof(Buffer));

	/*checking if the pointer returned by calloc failed*/
	if (!buff) {
		free(buff);
		return NULL;
	}

	/*init_capacity mustr be between 0 and MAX_ALLOWED_POSITIVE_VALUE -12 INCLUSIVE*/
	if (init_capacity >= 0 && init_capacity <= MAX_SHRT_VALUE_ALLOWED) {
		/*if it is 0*/
		if (init_capacity == 0) {
			/*sets buffer default size to 200*/
			init_capacity = DEFAULT_BUFFER_SIZE;
			/*default increment factor in mode a or m of 200*/
			if (o_mode == 'a' || o_mode == 'm') {
				inc_factor = DEFAULT_INC_FACTOR;

			}
			/*default increment factor in mode f of 0*/
			if (o_mode == 'f') {
				inc_factor = 0;

			}
			
		}
	}
	
	/*use malloc to create a character array on heap*/
	buffArr = (char*)malloc(sizeof(char) * init_capacity);

	/*checking if pointer retruned by malloc failed*/
	if (!buffArr) {
		free(buffArr);
		return NULL;
	}

	/*assigning that character array to the cb_head of the buffer structure*/
	buff->cb_head = buffArr;

	/*if mode is f , inc_factor is 0 and init_capacity is not 0*/
	if (o_mode == 'f' ) {
		if (inc_factor == 0 && init_capacity != 0) {
			buff->mode = 0;
			buff->inc_factor = 0;
		}
	}
	else

		/*if mode is a , and inc_factor is in between 1 and 255 INCLUSIVE*/
	if (o_mode == 'a') {
		if (inc_factor >= 1 && inc_factor <= 255) {
			buff->mode = 1;
			buff->inc_factor = inc_factor;
		}
	} else

	/*if mode is m , and inc_factor is in between 1 and 100 INCLUSIVE*/
	if (o_mode == 'm' && (inc_factor >= 1 && inc_factor <= 100)) {
		buff->mode = -1;
		buff->inc_factor = inc_factor;
	}

	/*setting buffer structure capacity to init_capacity*/
	buff->capacity = init_capacity;

	/*set flags to default */
	buff->flags = DEFAULT_FLAGS;

	/*return buffer structure*/
	return buff;
}
/*
* Purpose: This function tries to allocate memory for a BufferDescriptor
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions: realloc()
* Parameters:  pBD,  symbol
* Return value: BufferDescriptor
* Algorithm: checks validity, reset r_flag, check if buffer is full, if yes, will be resized
* according to the mode. If mode is 0, it returns null. If mode is 1, new capacity is equal
* to capacity plus its increment factor if its positive and smaller than max value. if it goes over
* max value, capacity will equal the max value. If it is negative, it will return null.
* if mode is -1 , if capacity exceeds max value, it will return null. if not, the new space available
* will equal the maximum capacity minus current capacity. The new increment will equal the available
* space times the inc_factor divided by 100. The new capacity is equal to current capacity
* plus the new increment. If capacity was incrmeneted successfully, no further adjustments. If
* the current capacity cannot be incremented, the capacity will be equal to max value. Set the old address
* to the cb_head. Using realloc to allocate new memory for new address. If unsuccessful, return
* NULL. Setting the cb_head to the new address. Setting capacity to new capacity. Sets the r_flag
* if the memory location has changed. Lastly, adds the symbol to the character buffer and returning
* and returning the character buffer.
*/
pBuffer b_addc(pBuffer const pBD, char symbol) {
	if (pBD) {
		/*variables*/
		short newCapacity = 0;
		short availableSpace = 0;
		short newIncerment = 0;
		char* newAddress;
		/*resetting r_flag*/
		pBD->flags &= RESET_R_FLAG;

		/*check if buffer is full*/
		if (pBD->addc_offset == pBD->capacity) {
			/*based on the mode*/
			if (pBD->mode == 0) {/*FIXED MODE*/
				return NULL;
			}

			if (pBD->mode == 1) {/*ADDITIVE MODE*/
				
		
				if (newCapacity > 0 && newCapacity < MAX_SHRT_VALUE_ALLOWED) {
					newCapacity = pBD->capacity + (unsigned char)pBD->inc_factor;
					
				}
				 if (newCapacity > 0 || newCapacity < MAX_SHRT_VALUE_ALLOWED) {
					newCapacity = MAX_SHRT_VALUE_ALLOWED;
				}
				if (newCapacity < 0) {/*negative*/
					return NULL;
				}
			
			}
			else

			if (pBD->mode == -1) {/*MULTIPLICATIVE MODE*/
				if (pBD->capacity == MAX_SHRT_VALUE_ALLOWED) {
					return NULL;
				}

				availableSpace = MAX_SHRT_VALUE_ALLOWED - (pBD->capacity);

				newIncerment = (short)(availableSpace * (((double)pBD->inc_factor) / HUNDRED));

				if (newIncerment == 0) {/*cannot be incremented*/
					newCapacity = MAX_SHRT_VALUE_ALLOWED;
				}
				else {/* can be*/
					newCapacity = pBD->capacity + newIncerment;
				}
			}
			
			newAddress = (char*)realloc(pBD->cb_head, sizeof(char) * (unsigned short)newCapacity);
			if (!newAddress) {/*checking validity*/
				return NULL;
			}
			if (newAddress != pBD->cb_head) {/*comparing both addresses*/
				pBD->flags &= RESET_R_FLAG;
				/*first reset then set*/
				pBD->flags |= SET_R_FLAG;
				pBD->cb_head = newAddress;/*assigning new address*/
				pBD->capacity = newCapacity;
			}
			
		}
		
		pBD->cb_head[pBD->addc_offset] = symbol;/*storing symbol in the buffer*/
		pBD->addc_offset++;/*incremeneting*/
		return pBD;
	}
	else {
		return NULL;
	}
}

/*
* Purpose: This function clears data members of an instance of BufferDescriptor
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: int
* Algorithm: checks validity, sets markc_offset, addc_offset, getc_offset to 0, resets flag to default. 
* returns if succesful and -1 if not. sets flags to default
*/
int b_clear(Buffer* const pBD) {
	if (pBD) {/*check for validity*/
		/*setting data members to 0*/
		pBD->markc_offset = 0;
		pBD->addc_offset = 0;
		pBD->getc_offset = 0;
		pBD->flags = DEFAULT_FLAGS;
		return 1;
	}
	else {/*fail*/
		return RT_FAIL_1;
	}
}

/*
* Purpose: This function frees cb_head of an instance of BufferDescriptor and then the instance
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions: free()
* Parameters:  pBD
* Return value: void
* Algorithm: checks validity, frees head, frees buffer
*/
void b_free(Buffer* const pBD) {
	free(pBD->cb_head);/*free the head of the buffer*/
	free(pBD);/*free the buffer*/
}

/*
* Purpose: Function checks if buffer is full 1 will be returned, 0 if not
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: in
* Algorithm: checks validity, checks if the buffers capacity is equal to the addc_offset. If yes retrurns 1. If not, returns 0.
*/
int b_isfull(Buffer* const pBD) {
	if (pBD) {/*checks validity*/
		if (b_capacity(pBD) == pBD->addc_offset) {/*if capacity is equal to addc_offset the buffer is full*/
			return 1;
		}
		else {/*if not full*/
			return 0;
		}
	}
	else {/*fail*/
		return RT_FAIL_1;
	}
}

short b_addcoffset(Buffer* const pBD) {
	if (pBD) {/*checks validity*/
		return pBD->addc_offset;
	}
	else {/*if fails*/
		return RT_FAIL_1;
	}
	
}

/*
* Purpose: Function returns capacity of character buffer
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: short
* Algorithm: checks validity, returns capacity if successful, if not returns -1.
*/
short b_capacity(Buffer* const pBD) {
	if (pBD) {/*error check*/
		return pBD->capacity;
	}
	else {/*error*/
		return RT_FAIL_1;
	}
}

/*
* Purpose: Function sets markc_offset to mark, with conditions
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: char
* Algorithm: checks validity, if mark is bigger than 0 and smaller than addc_offset (inclusive),
* set markc_offset to mark, return markc_offset. If validity fails, returns null.
*/
short b_markc(pBuffer const pBD, short mark) {
	if (pBD && (mark >= 0 && mark <= pBD->addc_offset)) {/*checks for error and range*/
		pBD->markc_offset = mark;/*set buffer markc_offset to mark parameter*/
		return pBD->markc_offset;
	}
	else {/*error or out of range*/
		return RT_FAIL_1;
	}
}

/*
* Purpose: Function returns the mode of an instance of BufferDescriptor
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: int
* Algorithm: checks validity, if successful, returns mode, if not, returns and prints to the console.
*/
int b_mode(Buffer* const pBD) {
	if (pBD) {/*error check*/
		return pBD->mode;
	}
	else {/*error*/
		printf("Error in b_mode function\n");/*message to user*/
		return RT_FAIL_1;
	}
}

/*
* Purpose: Function returns a positive inc_factor of an instance of BufferDescriptor
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: size_t
* Algorithm: checks validity, if successful, returns inc_factor, if not, returns 0x100.
*/
size_t b_incfactor(Buffer* const pBD) {
	if (pBD) { /*error check*/
		return (size_t)abs(pBD->inc_factor);
	}
	else {/*error*/
		return INC_ERROR;
	}
}

/*
* Purpose: Function reads from a file one character at a time and adds to character buffer.
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions: feof(), b_addc(), fgetc(), ungetc()
* Parameters:  pBD, fi
* Return value: int
* Algorithm: checks validity, if successful, reads from file and one character at a time will add onto the
* character buffer. Checks if file is at end of file. If yes, then it breaks from the function.
* If not, each character is assigned to temp, b_addc gets called with instance of BufferDescriptor and
* temp. If null gets returned, ungetc gets called with temp and fi. LOAD_FAIL is returned. For every character,
* numchar gets incremented and then it is returned.
*/
int b_load(FILE* const fi, Buffer* const pBD) {

	char temp;/*stores temporary variable*/
	int numchar = 0;
	if (pBD && fi) {/*checking for validity*/
		while (!feof(fi)) {/*as long as it is not end of file*/
			temp = (char)fgetc(fi);/*gets character and assigns it to temp*/

			if (feof(fi)) {/*if end of file*/
				break;
			}
			
			if (!b_addc(pBD, temp)) {/*if function returns NULL*/
				ungetc(temp, fi);/*returns character*/
				return LOAD_FAIL;
			}

			numchar++;
		 }
		return numchar;
	}
	else {/*fail*/
		return RT_FAIL_1;
	}
}

/*
* Purpose: Function returns 1 or 0 depending on the addc_offset of an instance of BufferDescriptor
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: int
* Algorithm: checks validity, if successful, if addc_offset is 0; 1 will be returned
* if not, 0 will be returned, if unsuccessful, returns -1.
*/
int b_isempty(Buffer* const pBD) {
	if (pBD) {/*error check*/
		if (pBD->addc_offset == 0) {/*if addc_offset is 0*/
			return 1;
		}
		else {
			return 0;
		}
	}
	else {/*error*/
		return RT_FAIL_1;
	}
}

/*
* Purpose: Function used to read the Buffer
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: char
* Algorithm: checks validity, if successful,if getc_offset is equal to addc_offset, eob bit is set,
* 0 is returned. If not, eob bit is reset, getc_offset is incremented and returned. If unsuccessful, returns -2.
*/
char b_getc(Buffer* const pBD) {
	if (pBD) {/*check validity*/
		if (pBD->getc_offset == pBD->addc_offset) {/*check if equal*/
			pBD->flags &= RESET_EOB;/*first reset then set*/
			pBD->flags |= SET_EOB;
			return 0;
		}
		else {
			pBD->flags &= RESET_EOB;/*reset*/
		}
		return pBD->cb_head[pBD->getc_offset++];/*return incremented*/
	}
	else {/*fail*/
		return RT_FAIL_2;
	}
}

/*
* Purpose: Function returns values of flags field determined only by eob bit
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: int
* Algorithm: checks validity, if successful, returns eob bit. If unsuccessful, returns -1.
*/
int b_eob(Buffer* const pBD) {
	if (pBD) {/*check validity*/
		return (pBD->flags & CHECK_EOB);/*check eob flag*/
	}
	else {/*fail*/
		return RT_FAIL_1;
	}
}

/*
* Purpose: Function uses the printf to print Buffer character by character
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions: b_getc(), b_eob(), printf()
* Parameters:  pBD, nl
* Return value: int
* Algorithm: checks validity, if successful, b_getc gets assigned to temp.
* b_eob gets called and if it is true then it breaks from function.
* printf prints temp.
* This is done until b_eob is not equal to 2. if nl isn't 0, new line character is printed to console
* count is then returned.
* If unsuccessful, returns -1.
*/
int b_print(Buffer* const pBD, char nl) {
	if (pBD) {/*check validity*/
		/*variables*/
		char temp;/*will hold character*/
		int count = 0;/*counts the number of characters*/

		do {/*loop*/
			temp = b_getc(pBD);/*get character from buffer and store in temp*/

			if (b_eob(pBD) == 2) {/*if end of buffer*/
				break;
			}

			printf("%c",(char)temp);/*print*/

			count++;/*increment*/
		} while (b_eob(pBD) != 2);

		if (nl != 0) {
			printf("\n");
		}
		return count;
	}
	else {/*fail*/
		return RT_FAIL_1;
	}
}

/*
* Purpose: Function shirnks or expands buffer to new capacity
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions: realloc
* Parameters:  pBD, symbol
* Return value: Buffer*
* Algorithm: checks validity, if successful, capacity is the addc_offset +1 in bytes.
* realloc is used for new capacity. if realloc unsuccessful, NULL is returned.
* New address is assigned to  cb_head. capacity is set to newCapacity
* addc_offset is used to add the symbol at the end of the character buffer and is then incremented.
* rflag is set. returns the instance of BufferDescriptor.
*/
Buffer* b_compact(Buffer* const pBD, char symbol) {
	if (pBD) {
		/*variables*/
		char* newAddress;
		short newCapacity = 0;

		newCapacity = pBD->addc_offset + 1;

		newAddress = realloc(pBD->cb_head, sizeof(char)*newCapacity);
		pBD->capacity = newCapacity;

		if (!newAddress) {
			return NULL;
		}
		if (newAddress != pBD->cb_head) {
			pBD->flags &= RESET_R_FLAG;
			/*first reset then set*/
			pBD->flags |= SET_R_FLAG;
			pBD->cb_head = newAddress;/*assigning new address*/
		}

		pBD->cb_head[pBD->addc_offset] = symbol;/*storing symbol in the buffer*/
		pBD->addc_offset++;/*incremeneting*/
		return pBD;
	}
	else {
		return NULL;
	}
}

/*
* Purpose: Function returns values of flags field determined only by rflag bit
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: char
* Algorithm: checks validity, if successful, returns rflag bit. If unsuccessful, returns -1.
*/
char b_rflag(Buffer* const pBD) {
	if (pBD) {
		return (pBD->flags & CHECK_R_FLAG);
	}
	else {
		return RT_FAIL_1;
	}
}

/*
* Purpose: Function decrements getc_offset by 1
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: short
* Algorithm: checks validity, if successful, getc_offset is decremented.
* then is it returned. If unsuccessful, returns -1.
*/
short b_retract(Buffer* const pBD) {
	if (pBD) {/*error check*/
		return (pBD->getc_offset--);/*return decremented getc_offset*/
	}
	else {/*error*/
		return RT_FAIL_1;
	}
}

/*
* Purpose: Function sets getc_offset to current value of markc_offset
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: short
* Algorithm: checks validity, if successful, getc_offset is set to markc_offset and returns getc_offset.
* then is it returned. If unsuccessful, returns -1.
*/
short b_reset(Buffer* const pBD) {
	if (pBD) {/*checks validity*/
		pBD->getc_offset = pBD->markc_offset;
		return pBD->getc_offset;
	}
	else {/*fail*/
		return RT_FAIL_1;
	}
}

/*
* Purpose: Function returns getc_offset
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: short
* Algorithm: checks validity, if successful, returns getc_offset. If unsuccessful, returns -1.
*/
short b_getcoffset(Buffer* const pBD) {
	if (pBD) {/*checks for validity*/
		return pBD->getc_offset;
	}
	else {/*fail*/
		return RT_FAIL_1;
	}
}

/*
* Purpose: Function sets getc_offset and markc_offset to 0 so buffer can be re-read
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: int
* Algorithm: checks validity, if successful, getc_offset and markc_offset to 0. then returns 0.
* If unsuccessful, returns -1.
*/
int b_rewind(Buffer* const pBD) {
	if (pBD) {/*checking for validity*/
		pBD->getc_offset = 0;
		pBD->markc_offset = 0;
		return 0;
	}
	else {/*fail*/
		return RT_FAIL_1;
	}
}

/*
* Purpose: Function returns a pointer to the location of the character buffer indicated by loc_offset
* Author: Federico Fonseca
* History/Versions: 1
* Called Functions:
* Parameters:  pBD
* Return value: int
* Algorithm: checks validity, if successful, retruns cb_head plus the loc_offset.
* If unsuccessful, returns NULL.
*/
char* b_location(Buffer* const pBD, short loc_offset) {
	if (pBD) {
		return pBD->cb_head + loc_offset;
	}
	else {
		return NULL;
	}
}