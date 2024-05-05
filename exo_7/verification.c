// gcc -g verification.c
// gcc -g -pedantic -Wall -Wextra -Wformat=2 -Winit-self -Wcast-qual -Wcast-align -Wconversion -Wwrite-strings -Wstrict-prototypes -Wfloat-equal -Wshadow -Wredundant-decls -Wundef -Wbad-function-cast -Wold-style-definition -Wmissing-declarations -Wmissing-include-dirs -Wswitch-default -Wlogical-op -Wnested-externs -Wunreachable-code -Wpadded verification.c

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void pointRead(char *NomFichier,int NbPoints, int *XPoints, int *YPoints);
void imgWrite(char const *fileName,int XLen, int Ylen, int NbPoints,
              int * XPoints, int * YPoints, int Dezoom);

void pointRead(char *NomFichier,int NbPoints, int *XPoints, int *YPoints) {
  char     *NumeroFichier;
  FILE     *Fichier;
  int       Point;
  int       iterp;

  // Replace N by 0
  NumeroFichier = strchr (NomFichier, 'N');
  *NumeroFichier = '0';
  if ((Fichier = fopen(NomFichier, "rt")) == NULL) {
    fprintf(stderr, "Error open the file '%s'\n",NomFichier);
    exit(1);
  }
  for (iterp=0; iterp<NbPoints/2; iterp++) {
    fscanf(Fichier, "%d", &Point);
    XPoints[iterp] = Point;
  }
  fclose(Fichier);

  // Replace N by 2
  *NumeroFichier = '2';
  if ((Fichier = fopen(NomFichier, "rt")) == NULL) {
    fprintf(stderr, "Error open the file '%s'\n",NomFichier);
    exit(1);
  }
  for (iterp=0; iterp<NbPoints/2; iterp++) {
    fscanf(Fichier, "%d", &Point);
    YPoints[iterp] = 620 - Point;
  }
  fclose(Fichier);

  // Replace N by 1
  *NumeroFichier = '1';
  if ((Fichier = fopen(NomFichier, "rt")) == NULL) {
    fprintf(stderr, "Error open the file '%s'\n",NomFichier);
    exit(1);
  }
  for (iterp=NbPoints/2; iterp<NbPoints; iterp++) {
    fscanf(Fichier, "%d\n", &Point);
    XPoints[iterp] = Point;
  }
  fclose(Fichier);

  // Replace N by 3
  *NumeroFichier = '3';
  if ((Fichier = fopen(NomFichier, "rt")) == NULL) {
    fprintf(stderr, "Error open the file '%s'\n",NomFichier);
    exit(1);
  }
  for (iterp=NbPoints/2; iterp<NbPoints; iterp++) {
    fscanf(Fichier, "%d", &Point);
    YPoints[iterp] = 620 - Point;
  }
  fclose(Fichier);
}

void imgWrite(char const *fileName,int XLen, int YLen, int NbPoints,
              int * XPoints, int * YPoints, int Dezoom) {
  FILE     *Fichier;
  int       img[YLen][XLen];
  int       iterp,iterx,itery,iy,ix;
  //
  if ((Fichier = fopen(fileName,"w")) == NULL) {
    fprintf(stderr, "Error open new file '%s'\n",fileName);
    exit(2);
  }
  fprintf(Fichier,"P3\n");
  fprintf(Fichier,"%d %d\n",XLen/Dezoom,YLen/Dezoom);
  fprintf(Fichier,"1\n");

  // White background
  for (itery=0;itery<YLen;itery++) {
    for (iterx=0;iterx<XLen;iterx++) {
      img[itery][iterx] = 1;
    }
  }

  // Trace line between points
  for (iterp=0; iterp<NbPoints-1; iterp++) {
    int x0,x1,y0,y1;
    int dx,dy,sx,sy;
    int err,e2;
    x0 = XPoints[iterp];
    x1 = XPoints[iterp+1];
    y0 = YPoints[iterp];
    y1 = YPoints[iterp+1];
    dx = abs(x1-x0);
    sx = x0<x1 ? 1 : -1;
    dy = abs(y1-y0);
    sy = y0<y1 ? 1 : -1;
    err = (dx>dy ? dx : -dy)/2;

    for(;;) {
      img[y0][x0] = 0;
      if (x0==x1 && y0==y1) break;
      e2 = err;
      if (e2 > -dx) { err -= dy; x0 += sx; }
      if (e2 < dy) { err += dx; y0 += sy; }
    }
  }

  // Write in ppm file
  for(itery=0;itery<YLen/Dezoom;itery++) {
    for(iterx=0;iterx<XLen/Dezoom;iterx++) {
      int color;
      color = 0;

      for (iy=0;iy<Dezoom; iy++) {
        for (ix=0;ix<Dezoom; ix++) {
          color = color+img[Dezoom*itery+iy][Dezoom*iterx+ix];
        }
      }
      color = color/(Dezoom*Dezoom);

      fprintf(Fichier, "%d %d %d\n",color,color,color);
    }
  }

  fclose(Fichier);
  fprintf(stdout,"%s generated\n", fileName);
}


int main(void) {
  char      NomFichier[17];
  int const NbPoints = 242;
  int       XPoints[NbPoints];
  int       YPoints[NbPoints];
  int const XLen = 650;
  int const YLen = XLen;
  int const Dezoom = 4; // Number of Point by pixel

  /* Read via explicit offsets, in individual mode */
  strcpy(NomFichier, "fichier_deiN.dat");
  pointRead(NomFichier, NbPoints, XPoints, YPoints);
  imgWrite("img01.ppm",XLen,YLen,NbPoints,XPoints,YPoints,Dezoom);

  /* Read via shared file pointers, in collective mode */
  strcpy(NomFichier, "fichier_ppcN.dat");
  pointRead(NomFichier, NbPoints, XPoints, YPoints);
  imgWrite("img02.ppm",XLen,YLen,NbPoints,XPoints,YPoints,Dezoom);

  /* Read via individual pointers, in individual mode */
  strcpy(NomFichier, "fichier_piiN.dat");
  pointRead(NomFichier, NbPoints, XPoints, YPoints);
  imgWrite("img03.ppm",XLen,YLen,NbPoints,XPoints,YPoints,Dezoom);

  /* Read via shared file pointers, in individual mode */
  strcpy(NomFichier, "fichier_ppiN.dat");
  pointRead(NomFichier, NbPoints, XPoints, YPoints);
  imgWrite("img04.ppm",XLen,YLen,NbPoints,XPoints,YPoints,Dezoom);

  return 0;
}

