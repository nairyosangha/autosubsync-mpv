#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <png.h>
#include <string.h>

void setRGB(png_byte *ptr, u_int8_t *rgb_ptr);
int writeImage(char* filename, u_int16_t width, u_int16_t height, u_int8_t *buffer, char* title);


int main(int argc, char *argv[])
{
	if (argc < 2) {
		fprintf(stderr, "Please specify at least 1 input file!\n");
		return 1;
	}

	char *inputFile;
	FILE *INPUT;
	u_int8_t *pixelBuffer;
	u_int16_t width, height;

	for (int i=1; i < argc; i++) {
		inputFile = argv[i];
		char outputFile[strlen(inputFile)];
		strncpy(outputFile, inputFile, strlen(inputFile));
		strncpy(&outputFile[strlen(inputFile) - 3], "png", 4);
		struct stat sb;
		if (stat(inputFile, &sb) == -1) {
			perror("stat");
			exit(1);
		}
		INPUT = fopen(inputFile, "rb");
		pixelBuffer = malloc(sb.st_size);
		fread(&width, 2, 1, INPUT);
		fread(&height, 2, 1, INPUT);
		fread(pixelBuffer, sb.st_size - 4, 1, INPUT);

		printf("[%s]: %ld bytes read --> saving to [%s]\n", inputFile, sb.st_size, outputFile);
		if (writeImage(outputFile, width, height, pixelBuffer, "sub image")) {
			goto cleanup;
			return 1;
		}
cleanup:
		free(pixelBuffer);
		fclose(INPUT);
	}
	return 0;
}

void setRGB(png_byte *ptr, u_int8_t *rgb)
{
	for (int i = 0; i<4; i++)
		ptr[i] = *(rgb+i);
}

int writeImage(char* filename, u_int16_t width, u_int16_t height, u_int8_t *pixelBuffer, char* title)
{
	int code = 1, bytes_per_pixel = 4;
	FILE *fp = NULL;
	png_structp png_ptr = NULL;
	png_infop info_ptr = NULL;
	png_bytep row = NULL;
	
	fp = fopen(filename, "wb");
	if (!fp) {
		fprintf(stderr, "Could not open file %s for writing\n", filename);
		goto cleanup;
	}

	png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (!png_ptr) {
		fprintf(stderr, "Could not allocate write struct\n");
		goto cleanup;
	}

	info_ptr = png_create_info_struct(png_ptr);
	if (!info_ptr) {
		fprintf(stderr, "Could not allocate info struct\n");
		goto cleanup;
	}

	// Setup Exception handling
	if (setjmp(png_jmpbuf(png_ptr))) {
		fprintf(stderr, "Error during png creation\n");
		goto cleanup;
	}

	png_init_io(png_ptr, fp);

	// Write header (8 bit colour depth)
	png_set_IHDR(png_ptr, info_ptr, width, height,
			8, PNG_COLOR_TYPE_RGBA, PNG_INTERLACE_NONE,
			PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_BASE);

	if (title != NULL) {
		png_text title_text;
		title_text.compression = PNG_TEXT_COMPRESSION_NONE;
		title_text.key = "Title";
		title_text.text = title;
		png_set_text(png_ptr, info_ptr, &title_text, 1);
	}

	png_write_info(png_ptr, info_ptr);

	row = (png_bytep) malloc(bytes_per_pixel * width * sizeof(png_byte));

	int x, y;
	for (y=0 ; y<height ; y++) {
		for (x=0 ; x<width ; x++) {
			setRGB(&(row[x*bytes_per_pixel]), &pixelBuffer[bytes_per_pixel*(y*width+x)]);
		}
		png_write_row(png_ptr, row);
	}

	// End write
	png_write_end(png_ptr, NULL);
	code = 0;

cleanup:
	if (fp != NULL) fclose(fp);
	if (info_ptr != NULL) png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);
	if (png_ptr != NULL) png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
	if (row != NULL) free(row);

	return code;
}
