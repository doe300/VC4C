/*
	RenderGirl - OpenCL raytracer renderer
	Copyright (c) 2014, Délio Lustosa Cantarelli, All rights reserved.

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 3.0 of the License, or any later version.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
	Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library.
*/


/*----------FXAA quality defines-----------
-------------------------------------------
-----------------------------------------*/
#define FXAA_EDGE_THRESHOLD	1/8		//The minimum amount of local contrast required to apply algorithm. 1 / 3 – too little, 1 / 4 – low quality, 1 / 8 – high quality, 1 / 16 – overkill
#define FXAA_EDGE_THRESHOLD_MIN 1/16	//Trims the algorithm from processing darks. 1 / 32 – visible limit, 1 / 16 – high quality, 1 / 12 – upper limit(start of visible unfiltered edges)
#define FXAA_SUBPIX_TRIM_SCALE 0 //Toggle subpix filtering. 0 – turn off, 1 – turn on, 2 – turn on force full (ignore FXAA_SUBPIX_TRIM and CAP)
#define FXAA_SUBPIX_TRIM 1/4 //Controls removal of sub-pixel aliasing., 1/2 – low removal, 1/3 – medium removal, 1/4 – default removal, 1/8 – high removal, 0 – complete removal
#define FXAA_SUBPIX_CAP 3/4 //Insures fine detail is not completely removed., This partly overrides FXAA_SUBPIX_TRIM., 3/4 – default amount of filtering, 7/8 – high amount of filtering, 1 – no capping of filtering
#define FXAA_SEARCH_STEPS 1 //Controls the maximum number of search steps., Multiply by FXAA_SEARCH_ACCELERATION for filtering radius., 
#define FXAA_SEARCH_ACCELERATION 1	//How much to accelerate search using anisotropic filtering., 1 – no acceleration, 2 – skip by 2 pixels, 3 – skip by 3 pixels, 4 – skip by 4 pixels (hard upper limit)
#define FXAA_SEARCH_THRESHOLD 1/4 //Controls when to stop searching. 1/4 – seems best quality wise




float FxaaLuma(uchar3 rgb) {
	return (float)rgb.y * 1.96321f + (float)rgb.x;
}

__kernel void AntiAliasingFXAA(__global uchar4* screenInput, __global uchar4* screenOutput, __global int* width, __global int* height)
{


	int id = get_global_id(0);
	int x = id % *width;				//-----column in which is the pixel
	int y = id / *width;				//-----line in which is the pixel

	bool canUp = true;
	bool canDown = true;
	bool canWest = true;
	bool canEast = true;

	//uchar4 nada = screenInput[id];

	//screenOutput[id] = convert_uchar4(convert_float4(nada) * (float4)(0.2f, 0.2f, 0.2f, 1.0f));
	//return;

	uchar3 rgbM = screenInput[id].xyz;
	//get current pixel and transform
	float lumaM = FxaaLuma(rgbM);

	uchar3 rgbW = rgbM;
	uchar3 rgbE = rgbM;
	uchar3 rgbN = rgbM;
	uchar3 rgbS = rgbM;
	float lumaW = lumaM;
	float lumaE = lumaM;
	float lumaN = lumaM;
	float lumaS = lumaM;

	//find the pixel of each coordinate and transform
	if (x > 0)
	{
		rgbW = screenInput[id - 1].xyz;
		lumaW = FxaaLuma(rgbN);
	}
	else
	{
		screenOutput[id] = screenInput[id];
		return;
	}
	if (x < *width - 1)
	{
		rgbE = screenInput[id + 1].xyz;
		lumaE = FxaaLuma(rgbE);
	}
	else
	{
		screenOutput[id] = screenInput[id];
		return;
	}
	if (y > 0)
	{
		rgbN = screenInput[id - *width].xyz;
		lumaN = FxaaLuma(rgbN);
	}
	else
	{
		screenOutput[id] = screenInput[id];
		return;
	}
	if (y < *height - 1)
	{
		rgbS = screenInput[id + *width].xyz;
		lumaS = FxaaLuma(rgbS);
	}
	else
	{
		screenOutput[id] = screenInput[id];
		return;
	}
	//uchar4 teste;
	//teste.xyz = convert_uchar3(convert_float3(rgbM + rgbW + rgbE + rgbN + rgbS) * (float3)(0.2f, 0.2f, 0.2f));
	//teste.w = 255;
	//screenOutput[id] =(uchar4)teste;
	//return;

	float rangeMin = min(lumaM, min(min(lumaN, lumaW), min(lumaS, lumaE)));
	float rangeMax = max(lumaM, max(max(lumaN, lumaW), max(lumaS, lumaE)));
	float range = rangeMax - rangeMin;
	if (range <
		max((float)FXAA_EDGE_THRESHOLD_MIN, rangeMax * FXAA_EDGE_THRESHOLD)) {
		screenOutput[id] = screenInput[id];
		return;	}	float lumaL = (lumaN + lumaW + lumaE + lumaS) * 0.25;
	int rangeL = fabs(lumaL - lumaM);
	float blendL = max(0.0f,
		(rangeL / range) - FXAA_SUBPIX_TRIM) * FXAA_SUBPIX_TRIM_SCALE;
	blendL = min((float)FXAA_SUBPIX_CAP, blendL);	float3 rgbL = convert_float3(rgbM);
	rgbL += convert_float3(rgbN);
	rgbL += convert_float3(rgbW);
	rgbL += convert_float3(rgbE);
	rgbL += convert_float3(rgbS);

	////////fazer condição pra cada direção!!!!
	uchar3 rgbNW = rgbM;
	uchar3 rgbNE = rgbM;
	uchar3 rgbSW = rgbM;
	uchar3 rgbSE = rgbM;
	float lumaNW = lumaM;
	float lumaNE = lumaM;
	float lumaSW = lumaM;
	float lumaSE = lumaM;


	rgbNW = screenInput[id - *width - 1].xyz;
	lumaNW = FxaaLuma(rgbNW);

	rgbNE = screenInput[id - *width + 1].xyz;
	lumaNE = FxaaLuma(rgbNE);

	rgbSW = screenInput[id + *width - 1].xyz;
	lumaSW = FxaaLuma(rgbSW);

	rgbSE = screenInput[id + *width + 1].xyz;
	lumaSE = FxaaLuma(rgbSE);


	rgbL += convert_float3(rgbNW);
	rgbL += convert_float3(rgbNE);
	rgbL += convert_float3(rgbSW);
	rgbL += convert_float3(rgbSE);
	rgbL = rgbL * (float3)(0.1111f, 0.1111f, 0.1111f);			//dont know if it's possible
	screenOutput[id].x = (uchar)rgbL.x;
	screenOutput[id].y = (uchar)rgbL.y;
	screenOutput[id].z = (uchar)rgbL.z;
	screenOutput[id].w = 255;
	//this part cannot be done without texture!!	/*-------------------------------------------
	//edge test
	float edgeVert =
	abs((0.25 * lumaNW) + (-0.5 * lumaN) + (0.25 * lumaNE)) +
	abs((0.50 * lumaW) + (-1.0 * lumaM) + (0.50 * lumaE)) +
	abs((0.25 * lumaSW) + (-0.5 * lumaS) + (0.25 * lumaSE));
	float edgeHorz =
	abs((0.25 * lumaNW) + (-0.5 * lumaW) + (0.25 * lumaSW)) +
	abs((0.50 * lumaN) + (-1.0 * lumaM) + (0.50 * lumaS)) +
	abs((0.25 * lumaNE) + (-0.5 * lumaE) + (0.25 * lumaSE));
	bool horzSpan = edgeHorz >= edgeVert;	float3 posN;	float3 posP;	float lumaEndN;	float lumaEndP;	lumaN = lumaM;	float gradientN = FXAA_SEARCH_THRESHOLD;	if (horzSpan)	{	posN = rgbW;	posP = rgbE;	}	else	{	posN = rgbS;	posP = rgbM;	}
	bool doneN = false;
	bool doneP = false;

	//search for edges
	for (uint i = 0; i < FXAA_SEARCH_STEPS; i++) {
	#if FXAA_SEARCH_ACCELERATION == 1					//MUST BE 1!!
	if (!doneN) lumaEndN = FxaaLuma(posN.xyz);
	if (!doneP) lumaEndP = FxaaLuma(posP.xyz);
	#else
	if (!doneN) lumaEndN = FxaaLuma(
	FxaaTextureGrad(tex, posN.xy, offNP).xyz);
	if (!doneP) lumaEndP = FxaaLuma(
	FxaaTextureGrad(tex, posP.xy, offNP).xyz);
	#endif
	doneN = doneN || (abs(lumaEndN - lumaN) >= gradientN);
	doneP = doneP || (abs(lumaEndP - lumaN) >= gradientN);
	if (doneN && doneP) break;
	if (!doneN) posN -= offNP;
	if (!doneP) posP += offNP;
	}
	------------------------------------------------*/

}
