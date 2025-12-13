/**
 * Dis VM - Bytecode Loader
 * Loads and validates Dis bytecode files
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define DIS_MAGIC 0x44495300  /* "DIS\0" */

typedef struct dis_bytecode_header {
    uint32_t magic;
    uint32_t version;
    uint32_t code_size;
    uint32_t data_size;
    uint32_t entry_point;
} dis_bytecode_header_t;

/**
 * Load bytecode from file
 */
int dis_bytecode_load_file(const char *filename, uint8_t **bytecode, size_t *size) {
    if (filename == NULL || bytecode == NULL || size == NULL) {
        return -1;
    }
    
    FILE *fp = fopen(filename, "rb");
    if (fp == NULL) {
        fprintf(stderr, "Failed to open bytecode file: %s\n", filename);
        return -1;
    }
    
    /* Get file size */
    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    
    if (file_size <= 0) {
        fprintf(stderr, "Invalid bytecode file size\n");
        fclose(fp);
        return -1;
    }
    
    /* Allocate buffer */
    *bytecode = (uint8_t *)malloc(file_size);
    if (*bytecode == NULL) {
        fprintf(stderr, "Failed to allocate bytecode buffer\n");
        fclose(fp);
        return -1;
    }
    
    /* Read file */
    size_t bytes_read = fread(*bytecode, 1, file_size, fp);
    fclose(fp);
    
    if (bytes_read != (size_t)file_size) {
        fprintf(stderr, "Failed to read bytecode file\n");
        free(*bytecode);
        *bytecode = NULL;
        return -1;
    }
    
    *size = file_size;
    
    printf("Loaded bytecode: %s (%zu bytes)\n", filename, *size);
    
    return 0;
}

/**
 * Validate bytecode
 */
int dis_bytecode_validate(const uint8_t *bytecode, size_t size) {
    if (bytecode == NULL || size < sizeof(dis_bytecode_header_t)) {
        return -1;
    }
    
    const dis_bytecode_header_t *header = (const dis_bytecode_header_t *)bytecode;
    
    /* Check magic number */
    if (header->magic != DIS_MAGIC) {
        fprintf(stderr, "Invalid bytecode magic number\n");
        return -1;
    }
    
    /* Check version */
    if (header->version > 1) {
        fprintf(stderr, "Unsupported bytecode version: %u\n", header->version);
        return -1;
    }
    
    /* Check sizes */
    size_t total_size = sizeof(dis_bytecode_header_t) + 
                        header->code_size + 
                        header->data_size;
    
    if (total_size > size) {
        fprintf(stderr, "Invalid bytecode size\n");
        return -1;
    }
    
    printf("Bytecode validated successfully\n");
    printf("  Version: %u\n", header->version);
    printf("  Code size: %u bytes\n", header->code_size);
    printf("  Data size: %u bytes\n", header->data_size);
    printf("  Entry point: 0x%08x\n", header->entry_point);
    
    return 0;
}

/**
 * Free bytecode
 */
void dis_bytecode_free(uint8_t *bytecode) {
    if (bytecode != NULL) {
        free(bytecode);
    }
}
