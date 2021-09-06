#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <png.h>
#include <string.h>

void setRGB(png_byte *ptr, u_int8_t *rgb_ptr);
int writeImage(char* filename, int width, int height, u_int8_t *buffer, char* title);


int main(int argc, char *argv[])
{
	if (argc < 2) {
		fprintf(stderr, "Please specify an input file\n");
		return 1;
	}
	for (int i=1; i < argc; i++) {
		char *fn = argv[i];
		int width, height;
		u_int64_t size = 0;
		u_int8_t *buffer = NULL;
		FILE *fj = fopen(fn, "rb");
		char dest[100]; memset(dest, '\0', 100);
		strncat(dest, fn, 96);
		strncat(dest, ".png", 4);
		fscanf(fj, "%d;%d\n", &width, &height);
		u_int8_t r, g, b;
		while (fscanf(fj, "%c%c%c", &r, &g, &b) == 3) {
			buffer = realloc(buffer, size + 3 * sizeof(u_int8_t));
			buffer[size + 0] = r;
			buffer[size + 1] = r;
			buffer[size + 2] = r;
			//printf("r: %d, g: %d, b: %d\n", r, g, b);
			size += 3;
		}
		printf("[%s]: %ld bytes read\n", fn, size);
		printf("\tdimensions: %dx%d\n", width, height);

		printf("Saving PNG to %s\n", dest);
		int result = writeImage(dest, width, height, buffer, "sub image");
		if (result) return result;
		printf("PNG saved succesfully!\n", dest);

		free(buffer);
		fclose(fj);
	}
	return 0;
}

void setRGB(png_byte *ptr, u_int8_t *rgb)
{
	ptr[0] = *(rgb+0);
	ptr[1] = *(rgb+1);
	ptr[2] = *(rgb+2);
}

int writeImage(char* filename, int width, int height, u_int8_t *buffer, char* title)
{
	int code = 0;
	FILE *fp = NULL;
	png_structp png_ptr = NULL;
	png_infop info_ptr = NULL;
	png_bytep row = NULL;
	
	fp = fopen(filename, "wb");
	if (!fp) {
		fprintf(stderr, "Could not open file %s for writing\n", filename);
		code = 1;
		goto finalise;
	}

	png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (!png_ptr) {
		fprintf(stderr, "Could not allocate write struct\n");
		code = 1;
		goto finalise;
	}

	info_ptr = png_create_info_struct(png_ptr);
	if (!info_ptr) {
		fprintf(stderr, "Could not allocate info struct\n");
		code = 1;
		goto finalise;
	}

	// Setup Exception handling
	if (setjmp(png_jmpbuf(png_ptr))) {
		fprintf(stderr, "Error during png creation\n");
		code = 1;
		goto finalise;
	}

	png_init_io(png_ptr, fp);

	// Write header (8 bit colour depth)
	// TODO use RGB with alpha channel
	png_set_IHDR(png_ptr, info_ptr, width, height,
			8, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE,
			PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_BASE);

	if (title != NULL) {
		png_text title_text;
		title_text.compression = PNG_TEXT_COMPRESSION_NONE;
		title_text.key = "Title";
		title_text.text = title;
		png_set_text(png_ptr, info_ptr, &title_text, 1);
	}

	png_write_info(png_ptr, info_ptr);

	row = (png_bytep) malloc(3 * width * sizeof(png_byte));

	int x, y;
	for (y=0 ; y<height ; y++) {
		for (x=0 ; x<width ; x++) {
			//printf("%d\n", 3*(y*width+x));
			setRGB(&(row[x*3]), &buffer[3*(y*width+x)]);
		}
		png_write_row(png_ptr, row);
	}

	// End write
	png_write_end(png_ptr, NULL);

	finalise:
	if (fp != NULL) fclose(fp);
	if (info_ptr != NULL) png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);
	if (png_ptr != NULL) png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
	if (row != NULL) free(row);

	return code;
}
