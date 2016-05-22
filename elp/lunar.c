/* From libnova */
/*
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 *  Copyright (C) 2000 - 2005 Liam Girdwood <lgirdwood@gmail.com>
 *
 *  The functions in this file use the Lunar Solution ELP 2000-82B by
 *  Michelle Chapront-Touze and Jean Chapront.
 */

#ifdef __GNUC__
#include <math.h>
#endif

#ifndef __GNUC__
#define M_PI 3.141592653589793
#define M_PI_2 (3.141592653589793 / 2.0)
#endif

#include "lunar-priv.h"
#include "elp.h"

/* puts a large angle in the correct range 0 - 2PI radians */
double ln_range_radians(double angle)
{
  double temp;

  if (angle >= 0.0 && angle < (2.0 * M_PI))
    return angle;
    
  //temp = (int)(angle / (M_PI * 2.0));
  temp = (angle / (M_PI * 2.0));

  if (angle < 0.0)
    temp --;
  temp *= (M_PI * 2.0);
  return angle - temp;
}

/* puts a large angle in the correct range -2PI - 2PI radians */
/* preserve sign */
double ln_range_radians2(double angle)
{
  double temp;
    
  if (angle > (-2.0 * M_PI) && angle < (2.0 * M_PI))
    return angle;
    
  //temp = (int)(angle / (M_PI * 2.0));
  temp = (angle / (M_PI * 2.0));
  temp *= (M_PI * 2.0);
  return angle - temp;
}

/* AU in KM */
#define AU			149597870

/* Chapront theory lunar constants */
#define		RAD		(648000.0 / M_PI)
#define		DEG		(M_PI / 180.0)
#define		M_PI2	(2.0 * M_PI)
#define		PIS2	(M_PI / 2.0)
#define		ATH		384747.9806743165
#define		A0		384747.9806448954
#define		AM		0.074801329518
#define		ALPHA	0.002571881335
#define		DTASM	(2.0 * ALPHA / (3.0 * AM))
#define		W12		(1732559343.73604 / RAD)
#define		PRECES	(5029.0966 / RAD)
#define		C1		60.0
#define		C2		3600.0

/* Corrections of the constants for DE200/LE200 */
#define		DELNU	((0.55604 / RAD) / W12)
#define		DELE	(0.01789 / RAD)
#define		DELG	(-0.08066 / RAD)
#define		DELNP	((-0.06424 / RAD) / W12)
#define		DELEP	(-0.12879 / RAD)

/*     Precession matrix */
#define		P1		0.10180391e-4
#define		P2		0.47020439e-6
#define		P3		-0.5417367e-9
#define		P4		-0.2507948e-11
#define		P5		0.463486e-14
#define		Q1		-0.113469002e-3
#define		Q2		0.12372674e-6
#define		Q3		0.1265417e-8
#define		Q4		-0.1371808e-11
#define		Q5		-0.320334e-14

/* constants with corrections for DE200 / LE200 */
static const double W1[5] =
{
	((218.0 + (18.0 / 60.0) + (59.95571 / 3600.0))) * DEG,
	1732559343.73604 / RAD,
	-5.8883 / RAD,
	0.006604 / RAD,
	-0.00003169 / RAD
};

static const double W2[5] =
{
	((83.0 + (21.0 / 60.0) + (11.67475 / 3600.0))) * DEG,
	14643420.2632 / RAD,
	-38.2776 /  RAD,
	-0.045047 / RAD,
	0.00021301 / RAD
};

static const double W3[5] =
{
	(125.0 + (2.0 / 60.0) + (40.39816 / 3600.0)) * DEG,
	-6967919.3622 / RAD,
	6.3622 / RAD,
	0.007625 / RAD,
	-0.00003586 / RAD
};

static const double earth[5] =
{
	(100.0 + (27.0 / 60.0) + (59.22059 / 3600.0)) * DEG,
	129597742.2758 / RAD,
	-0.0202 / RAD,
	0.000009 / RAD,
	0.00000015 / RAD
};

static const double peri[5] =
{
	(102.0 + (56.0 / 60.0) + (14.42753 / 3600.0)) * DEG,
	1161.2283 / RAD,
	0.5327 / RAD,
	-0.000138 / RAD,
	0.0
};

/* Delaunay's arguments.*/
static const double del[4][5] = {
	{ 5.198466741027443, 7771.377146811758394, -0.000028449351621, 0.000000031973462, -0.000000000154365 },
	{ -0.043125180208125, 628.301955168488007, -0.000002680534843, 0.000000000712676, 0.000000000000727 },
	{ 2.355555898265799, 8328.691426955554562, 0.000157027757616, 0.000000250411114, -0.000000001186339 },
	{ 1.627905233371468, 8433.466158130539043, -0.000059392100004, -0.000000004949948, 0.000000000020217 }
};


static const double zeta[2] =
{
	(218.0 + (18.0 / 60.0) + (59.95571 / 3600.0)) * DEG,
	((1732559343.73604 / RAD) + PRECES)
};


/* Planetary arguments */
static const double p[8][2] =
{
	{(252.0 + 15.0 / C1 + 3.25986 / C2 ) * DEG, 538101628.68898 / RAD },
	{(181.0 + 58.0 / C1 + 47.28305 / C2) * DEG, 210664136.43355 / RAD },
	{(100.0 + (27.0 / 60.0) + (59.22059 / 3600.0)) * DEG, 129597742.2758 / RAD},
	{(355.0 + 25.0 / C1 + 59.78866 / C2) * DEG, 68905077.59284 / RAD },
	{(34.0 + 21.0 / C1 + 5.34212 / C2) * DEG, 10925660.42861 / RAD },
	{(50.0 + 4.0 / C1 + 38.89694 / C2) * DEG, 4399609.65932 / RAD },
	{(314.0 + 3.0 / C1 + 18.01841 / C2) * DEG, 1542481.19393 / RAD },
	{(304.0 + 20.0 / C1 + 55.19575 / C2) * DEG, 786550.32074 / RAD }
};

/* sum lunar elp1 series */
static double sum_series_elp1 (double* t)
{
	double result = 0;
	double x,y;
	double tgv;
	int i,j,k;

	for (j = 0; j < ELP1_SIZE; j++) {
		/* derivatives of A */
		tgv = elp1[j].B[0] + DTASM * elp1[j].B[4];
		x = elp1[j].A + tgv * (DELNP - AM * DELNU) +
			elp1[j].B[1] * DELG + elp1[j].B[2] *
			DELE + elp1[j].B[3] * DELEP;

		y = 0;
		for (k = 0; k < 5; k++) {
			for (i = 0; i < 4; i++)
				y += elp1[j].ilu[i] * del[i][k] * t[k];
		}

		/* y in correct quad */
		y = ln_range_radians2(y);
		result += x * sin(y);
	}
	return result;
}

/* sum lunar elp2 series */
static double sum_series_elp2 (double* t)
{
	double result = 0;
	double x,y;
	double tgv;
	int i,j,k;

	for (j = 0; j < ELP2_SIZE; j++) {
		/* derivatives of A */
		tgv = elp2[j].B[0] + DTASM * elp2[j].B[4];
		x = elp2[j].A + tgv * (DELNP - AM * DELNU) +
			elp2[j].B[1] * DELG + elp2[j].B[2] *
			DELE + elp2[j].B[3] * DELEP;

		y = 0;
		for (k = 0; k < 5; k++) {
			for (i = 0; i < 4; i++)
				y += elp2[j].ilu[i] * del[i][k] * t[k];
		}
		/* y in correct quad */
		y = ln_range_radians2(y);
		result += x * sin(y);
	}
	return result;
}

/* sum lunar elp3 series */
static double sum_series_elp3 (double* t)
{
	double result = 0;
	double x,y;
	double tgv;
	int i,j,k;

	for (j = 0; j < ELP3_SIZE; j++) {
		/* derivatives of A */
		tgv = elp3[j].B[0] + DTASM * elp3[j].B[4];
		x = elp3[j].A + tgv * (DELNP - AM * DELNU) +
			elp3[j].B[1] * DELG + elp3[j].B[2] *
			DELE + elp3[j].B[3] * DELEP;

		y = 0;
		for (k = 0; k < 5; k++) {
			for (i = 0; i < 4; i++)
				y += elp3[j].ilu[i] * del[i][k] * t[k];
		}
		y += (M_PI_2);
		/* y in correct quad */
		y = ln_range_radians2(y);
		result += x * sin(y);
	}
	return result;
}


/* sum lunar elp4 series */
static double sum_series_elp4(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP4_SIZE; j++) {
		y = elp4[j].O * DEG;
		for (k = 0; k < 2; k++)  {
			y += elp4[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp4[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp4[j].A * sin(y);
	}
	return result;
}

/* sum lunar elp5 series */
static double sum_series_elp5(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP5_SIZE; j++) {
		y = elp5[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp5[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp5[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp5[j].A * sin(y);
	}
	return result;
}


/* sum lunar elp6 series */
static double sum_series_elp6(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP6_SIZE; j++) {
		y = elp6[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp6[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp6[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp6[j].A * sin(y);
	}
	return result;
}

/* sum lunar elp7 series */
static double sum_series_elp7(double *t)
{
	double result = 0;
	int i,j,k;
	double y, A;

	for (j = 0; j < ELP7_SIZE; j++) {
		A = elp7[j].A * t[1];
		y = elp7[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp7[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp7[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += A * sin(y);
	}
	return result;
}

/* sum lunar elp8 series */
static double sum_series_elp8(double *t)
{
	double result = 0;
	int i,j,k;
	double y, A;

	for (j = 0; j < ELP8_SIZE; j++) {
		y = elp8[j].O * DEG;
		A = elp8[j].A * t[1];
		for (k = 0; k < 2; k++) {
			y += elp8[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp8[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += A * sin(y);
	}
	return result;
}

/* sum lunar elp9 series */
static double sum_series_elp9(double *t)
{
	double result = 0;
	int i,j,k;
	double y, A;

	for (j = 0; j < ELP9_SIZE; j++) {
		A = elp9[j].A * t[1];
		y = elp9[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp9[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp9[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += A * sin(y);
	}
	return result;
}

/* sum lunar elp10 series */
static double sum_series_elp10(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP10_SIZE; j++) {
		y = elp10[j].theta * DEG;

		for (k = 0; k < 2; k++) {
			y += (elp10[j].ipla[8] * del[0][k]
			+ elp10[j].ipla[9] * del[2][k]
			+ elp10[j].ipla[10] * del [3][k]) * t[k];
			for (i = 0; i < 8; i++)
				y += elp10[j].ipla[i] * p[i][k] * t[k];
		}

		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp10[j].O * sin(y);
	}
	return result;
}

/* sum lunar elp11 series */
static double sum_series_elp11(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP11_SIZE; j++) {
		y = elp11[j].theta * DEG;
		for (k = 0; k < 2; k++)  {
			y += (elp11[j].ipla[8] * del[0][k]
			+ elp11[j].ipla[9] * del[2][k]
			+ elp11[j].ipla[10] * del [3][k]) * t[k];
			for (i = 0; i < 8; i++)
				y += elp11[j].ipla[i] * p[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp11[j].O * sin(y);
	}
	return result;
}

/* sum lunar elp12 series */
static double sum_series_elp12(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP12_SIZE; j++) {
		y = elp12[j].theta * DEG;
		for (k = 0; k < 2; k++) {
			y += (elp12[j].ipla[8] * del[0][k]
			+ elp12[j].ipla[9] * del[2][k]
			+ elp12[j].ipla[10] * del [3][k]) * t[k];
			for (i = 0; i < 8; i++)
				y += elp12[j].ipla[i] * p[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp12[j].O * sin(y);
	}
	return result;
}

/* sum lunar elp13 series */
static double sum_series_elp13(double *t)
{
	double result = 0;
	int i,j,k;
	double y,x;

	for (j = 0; j < ELP13_SIZE; j++) {
		y = elp13[j].theta * DEG;
		for (k = 0; k < 2; k++) {
			y += (elp13[j].ipla[8] * del[0][k]
			+ elp13[j].ipla[9] * del[2][k]
			+ elp13[j].ipla[10] * del [3][k]) * t[k];
			for (i = 0; i < 8; i++)
				y += elp13[j].ipla[i] * p[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		x = elp13[j].O * t[1];
		result += x * sin(y);
	}
	return result;
}

/* sum lunar elp14 series */
static double sum_series_elp14(double *t)
{
	double result = 0;
	int i,j,k;
	double y,x;

	for (j = 0; j < ELP14_SIZE; j++) {
		y = elp14[j].theta * DEG;
		for (k = 0; k < 2; k++)  {
			y += (elp14[j].ipla[8] * del[0][k]
			+ elp14[j].ipla[9] * del[2][k]
			+ elp14[j].ipla[10] * del [3][k]) * t[k];
			for (i = 0; i < 8; i++)
				y += elp14[j].ipla[i] * p[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		x = elp14[j].O * t[1];
		result += x * sin(y);
	}
	return result;
}


/* sum lunar elp15 series */
static double sum_series_elp15(double *t)
{
	double result = 0;
	int i,j,k;
	double y,x;

	for (j = 0; j < ELP15_SIZE; j++) {
		y = elp15[j].theta * DEG;
		for (k = 0; k < 2; k++) {
			y += (elp15[j].ipla[8] * del[0][k]
			+ elp15[j].ipla[9] * del[2][k]
			+ elp15[j].ipla[10] * del [3][k]) * t[k];
			for (i = 0; i < 8; i++)
				y += elp15[j].ipla[i] * p[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		x = elp15[j].O * t[1];
		result += x * sin(y);
	}
	return result;
}

/* sum lunar elp16 series */
static double sum_series_elp16(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP16_SIZE; j++) {
		y = elp16[j].theta * DEG;
		for (k = 0; k < 2; k++) {
			for (i = 0; i < 4; i++)
				y += elp16[j].ipla[i + 7] * del[i][k] * t[k];
			for (i = 0; i < 7; i++)
				y += elp16[j].ipla[i] * p[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp16[j].O * sin(y);
	}
	return result;
}

static double sum_series_elp17(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP17_SIZE; j++) {
		y = elp17[j].theta * DEG;
		for (k = 0; k < 2; k++) {
			for (i = 0; i < 4; i++)
				y += elp17[j].ipla[i + 7] * del[i][k] * t[k];
			for (i = 0; i < 7; i++)
				y += elp17[j].ipla[i] * p[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp17[j].O * sin(y);
	}
	return result;
}

static double sum_series_elp18(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP18_SIZE; j++) {
		y = elp18[j].theta * DEG;
		for (k = 0; k < 2; k++) {
			for (i = 0; i < 4; i++)
				y += elp18[j].ipla[i + 7] * del[i][k] * t[k];
			for (i = 0; i < 7; i++)
				y += elp18[j].ipla[i] * p[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp18[j].O * sin(y);
	}
	return result;
}

static double sum_series_elp19(double *t)
{
	double result = 0;
	int i,j,k;
	double y,x;

	for (j = 0; j < ELP19_SIZE; j++) {
		y = elp19[j].theta * DEG;
		for (k = 0; k < 2; k++) {
			for (i = 0; i < 4; i++)
				y += elp19[j].ipla[i + 7] * del[i][k] * t[k];
			for (i = 0; i < 7; i++)
				y += elp19[j].ipla[i] * p[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		x = elp19[j].O * t[1];
		result += x * sin(y);
	}
	return result;
}

static double sum_series_elp20(double *t)
{
	double result = 0;
	int i,j,k;
	double y,x;

	for (j = 0; j < ELP20_SIZE; j++) {
		y = elp20[j].theta * DEG;
		for (k = 0; k < 2; k++) {
			for (i = 0; i < 4; i++)
				y += elp20[j].ipla[i + 7] * del[i][k] * t[k];
			for (i = 0; i < 7; i++)
				y += elp20[j].ipla[i] * p[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		x = elp20[j].O * t[1];
		result += x * sin(y);
	}
	return result;
}

static double sum_series_elp21(double *t)
{
	double result = 0;
	int i,j,k;
	double y,x;

	for (j = 0; j < ELP21_SIZE; j++) {
		y = elp21[j].theta * DEG;
		for (k = 0; k < 2; k++) {
			for (i = 0; i < 4; i++)
				y += elp21[j].ipla[i + 7] * del[i][k] * t[k];
			for (i = 0; i < 7; i++)
				y += elp21[j].ipla[i] * p[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		x = elp21[j].O * t[1];
		result += x * sin(y);
	}
	return result;
}

/* sum lunar elp22 series */
static double sum_series_elp22(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP22_SIZE; j++) {
		y = elp22[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp22[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp22[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp22[j].A * sin(y);
	}
	return result;
}

/* sum lunar elp23 series */
static double sum_series_elp23(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP23_SIZE; j++) {
		y = elp23[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp23[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp23[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp23[j].A * sin(y);
	}
	return result;
}

/* sum lunar elp24 series */
static double sum_series_elp24(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP24_SIZE; j++) {
		y = elp24[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp24[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp24[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp24[j].A * sin(y);
	}
	return result;
}

/* sum lunar elp25 series */
static double sum_series_elp25(double *t)
{
	double result = 0;
	int i,j,k;
	double y, A;

	for (j = 0; j < ELP25_SIZE; j++) {
		A = elp25[j].A * t[1];
		y = elp25[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp25[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp25[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += A * sin(y);
	}
	return result;
}

/* sum lunar elp26 series */
static double sum_series_elp26(double *t)
{
	double result = 0;
	int i,j,k;
	double y, A;

	for (j = 0; j < ELP26_SIZE; j++) {
		A = elp26[j].A * t[1];
		y = elp26[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp26[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp26[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += A * sin(y);
	}
	return result;
}

/* sum lunar elp27 series */
static double sum_series_elp27(double *t)
{
	double result = 0;
	int i,j,k;
	double y, A;

	for (j = 0; j < ELP27_SIZE; j++) {
		A = elp27[j].A * t[1];
		y = elp27[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp27[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp27[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += A * sin(y);
	}
	return result;
}

/* sum lunar elp28 series */
static double sum_series_elp28(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP28_SIZE; j++) {
		y = elp28[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp28[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp28[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp28[j].A * sin(y);
	}
	return result;
}

/* sum lunar elp29 series */
static double sum_series_elp29(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP29_SIZE; j++) {
		y = elp29[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp29[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp29[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp29[j].A * sin(y);
	}
	return result;
}


/* sum lunar elp30 series */
static double sum_series_elp30(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP30_SIZE; j++) {
		y = elp30[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp30[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp30[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp30[j].A * sin(y);
	}
	return result;
}


/* sum lunar elp31 series */
static double sum_series_elp31(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP31_SIZE; j++) {
		y = elp31[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp31[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp31[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp31[j].A * sin(y);
	}
	return result;
}

/* sum lunar elp32 series */
static double sum_series_elp32(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP32_SIZE; j++) {
		y = elp32[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp32[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp32[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp32[j].A * sin(y);
	}
	return result;
}

/* sum lunar elp33 series */
static double sum_series_elp33(double *t)
{
	double result = 0;
	int i,j,k;
	double y;

	for (j = 0; j < ELP33_SIZE; j++) {
		y = elp33[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp33[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp33[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += elp33[j].A * sin(y);
	}
	return result;
}

/* sum lunar elp34 series */
static double sum_series_elp34(double *t)
{
	double result = 0;
	int i,j,k;
	double y, A;

	for (j = 0; j < ELP34_SIZE; j++) {
		A = elp34[j].A * t[2];
		y = elp34[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp34[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp34[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += A * sin(y);
	}
	return result;
}
/* sum lunar elp35 series */
static double sum_series_elp35(double *t)
{
	double result = 0;
	int i,j,k;
	double y, A;

	for (j = 0; j < ELP35_SIZE; j++) {
		A = elp35[j].A * t[2];
		y = elp35[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp35[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp35[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += A * sin(y);
	}
	return result;
}

/* sum lunar elp36 series */
static double sum_series_elp36(double *t)
{
	double result = 0;
	int i,j,k;
	double y, A;

	for (j = 0; j < ELP36_SIZE; j++) {
		A = elp36[j].A * t[2];
		y = elp36[j].O * DEG;
		for (k = 0; k < 2; k++) {
			y += elp36[j].iz * zeta[k] * t[k];
			for (i = 0; i < 4; i++)
				y += elp36[j].ilu[i] * del[i][k] * t[k];
		}
		/* put y in correct quad */
		y = ln_range_radians2(y);
		result += A * sin(y);
	}
	return result;
}

/*! \fn void ln_get_lunar_geo_posn(double JD, struct ln_rect_posn *pos, double precision);
* \param JD Julian day.
* \param pos Pointer to a geocentric position structure to held result.
* \param precision The truncation level of the series in radians for longitude
* and latitude and in km for distance. (Valid range 0 - 0.01, 0 being highest accuracy)
* \ingroup lunar
*
* Calculate the rectangular geocentric lunar coordinates to the inertial mean
* ecliptic and equinox of J2000.
* The geocentric coordinates returned are in units of km.
*
* This function is based upon the Lunar Solution ELP2000-82B by
* Michelle Chapront-Touze and Jean Chapront of the Bureau des Longitudes,
* Paris.
*/
/* ELP 2000-82B theory */
double moon[3];
double* ln_get_lunar_geo_posn(double jd)
{
	double ct[5];
	double elp[36];
	double a,b,c;
	double x,y,z;
	double pw,qw, pwqw, pw2, qw2, ra;

	/* calc julian centuries */
	ct[0] = 1.0;
	ct[1] = (jd - 2451545.0) / 36525.0;
	ct[2] = ct[1] * ct[1];
	ct[3] = ct[2] * ct[1];
	ct[4] = ct[3] * ct[1];

	/* sum elp series */
	elp[0] = sum_series_elp1(ct);
	elp[1] = sum_series_elp2(ct);
	elp[2] = sum_series_elp3(ct);
	elp[3] = sum_series_elp4(ct);
	elp[4] = sum_series_elp5(ct);
	elp[5] = sum_series_elp6(ct);
	elp[6] = sum_series_elp7(ct);
	elp[7] = sum_series_elp8(ct);
	elp[8] = sum_series_elp9(ct);
        // XXX 10,11,12 are very large and take a long time to compute
	elp[9] = 0.0; //sum_series_elp10(ct);
	elp[10] = 0.0; //sum_series_elp11(ct);
	elp[11] = 0.0; //sum_series_elp12(ct);
	elp[12] = sum_series_elp13(ct);
	elp[13] = sum_series_elp14(ct);
	elp[14] = sum_series_elp15(ct);
	elp[15] = sum_series_elp16(ct);
	elp[16] = sum_series_elp17(ct);
	elp[17] = sum_series_elp18(ct);
	elp[18] = sum_series_elp19(ct);
	elp[19] = sum_series_elp20(ct);
	elp[20] = sum_series_elp21(ct);
	elp[21] = sum_series_elp22(ct);
	elp[22] = sum_series_elp23(ct);
	elp[23] = sum_series_elp24(ct);
	elp[24] = sum_series_elp25(ct);
	elp[25] = sum_series_elp26(ct);
	elp[26] = sum_series_elp27(ct);
	elp[27] = sum_series_elp28(ct);
	elp[28] = sum_series_elp29(ct);
	elp[29] = sum_series_elp30(ct);
	elp[30] = sum_series_elp31(ct);
	elp[31] = sum_series_elp32(ct);
	elp[32] = sum_series_elp33(ct);
	elp[33] = sum_series_elp34(ct);
	elp[34] = sum_series_elp35(ct);
	elp[35] = sum_series_elp36(ct);

	a = elp[0] + elp[3] + elp[6] + elp[9] + elp[12] +
		elp[15] + elp[18] + elp[21] + elp[24] +
		elp[27] + elp[30] + elp[33];
	b = elp[1] + elp[4] + elp[7] + elp[10] + elp[13] +
		elp[16] + elp[19] + elp[22] + elp[25] +
		elp[28] + elp[31] + elp[34];
	c = elp[2] + elp[5] + elp[8] + elp[11] + elp[14] +
		elp[17] + elp[20] + elp[23] + elp[26] +
		elp[29] + elp[32] + elp[35];

	/* calculate geocentric coords */
	a = a / RAD + W1[0] + W1[1] * ct[1] + W1[2] * ct[2] + W1[3] * ct[3]
	    + W1[4] * ct[4];
	b = b / RAD;
	c = c * A0 / ATH;

	x = c * cos(b);
	y = x * sin(a);
	x = x * cos(a);
	z = c * sin(b);

	/* Laskars series */
	pw = (P1 + P2 * ct[1] + P3 * ct[2] + P4 * ct[3] + P5 * ct[4]) * ct[1];
	qw = (Q1 + Q2 * ct[1] + Q3 * ct[2] + Q4 * ct[3] + Q5 * ct[4]) * ct[1];
	ra = 2.0 * sqrt(1.0 - pw * pw - qw * qw);
	pwqw = 2.0 * pw * qw;
	pw2 = 1.0 - 2.0 * pw * pw;
	qw2 = 1.0 - 2.0 * qw * qw;
	pw = pw * ra;
	qw = qw * ra;
	a = pw2 * x + pwqw * y + pw * z;
	b = pwqw * x + qw2 * y - qw * z;
	c = -pw * x + qw * y + (pw2 + qw2 - 1.0) * z;

	/* save result */
	moon[0] = a;
	moon[1] = b;
	moon[2] = c;
        return moon;
}
