#include <sys/types.h>
#include <libgte.h>
#include <libetc.h>
#include <libgpu.h>
#include <libapi.h>
#include <stdlib.h>

#define OTLEN 8
#define MAX_SNAKE_LENGTH 300
DISPENV disp[2];
DRAWENV draw[2];
int db = 0;
u_long ot[2][OTLEN];
char pribuff[2][32768];
char *nextpri;
TILE *tile;
#define PAD_SELECT      1
#define PAD_L3          2
#define PAD_R3          4
#define PAD_START       8
#define PAD_UP          16
#define PAD_RIGHT       32
#define PAD_DOWN        64
#define PAD_LEFT        128
#define PAD_L2          256
#define PAD_R2          512
#define PAD_L1          1024
#define PAD_R1          2048
#define PAD_TRIANGLE    4096
#define PAD_CIRCLE      8192
#define PAD_CROSS       16384
#define PAD_SQUARE      32768
typedef struct _PADTYPE
{
   unsigned char	stat;
   unsigned char	len:4;
   unsigned char	type:4;
   unsigned short	btn;
   unsigned char	rs_x,rs_y;
   unsigned char	ls_x,ls_y;
} PADTYPE;
u_char padbuff[2][34];
void init(void) {
   ResetGraph(0);
   SetDefDispEnv(&disp[0], 0, 0, 320, 240);
   SetDefDispEnv(&disp[1], 0, 240, 320, 240);
   SetDefDrawEnv(&draw[0], 0, 240, 320, 240);
   SetDefDrawEnv(&draw[1], 0, 0, 320, 240);
   setRGB0(&draw[0], 63, 0, 127);
   setRGB0(&draw[1], 63, 0, 127);
   draw[0].isbg = 1;
   draw[1].isbg = 1;
   PutDispEnv(&disp[0]);
   PutDrawEnv(&draw[0]);
   nextpri = pribuff[0];
   InitPAD( padbuff[0], 34, padbuff[1], 34 );
   StartPAD();
   ChangeClearPAD( 1 );
   FntLoad(960, 0);
   FntOpen(0, 16, 320, 224, 0, 100);
}
void display(void) {
   DrawSync(0);
   VSync(0);
   PutDispEnv(&disp[db]);
   PutDrawEnv(&draw[db]);
   SetDispMask(1);
   DrawOTag(ot[db]+OTLEN-1);
   db = !db;
   nextpri = pribuff[db];
}
typedef struct Color{
   int r;
   int g;
   int b;
} Color;
typedef struct GameObject {
   int x;
   int y;
   int width;
   int height;
   Color color;
} GameObject;
Color RED = {255, 0, 0};
Color BLUE = {0, 0, 255};
Color GREEN = {0, 255, 0};
Color YELLOW = {255, 255, 0};
Color WHITE = {255, 255, 255};
void renderGameObject(GameObject* object) {
  tile = (TILE*)nextpri;      // Cast next primitive
  setTile(tile);              // Initialize the primitive (very important)
  setXY0(tile, object->x, object->y);       // Set primitive (x,y) position
  setWH(tile, object->width, object->height);        // Set primitive size
  setRGB0(tile, object->color.r, object->color.g, object->color.b); // Set color yellow
  addPrim(ot[db], tile);      // Add primitive to the ordering table
  nextpri += sizeof(TILE);    // Advance the next primitive pointer
}


typedef struct Engine{
}Engine;


typedef struct Car{
GameObject gameObject;

int does_incr;

int speed;

int increment;

int acc;

Engine engine1;
}Car;




Engine initializeEngine() {
Engine varEngine;
return varEngine;
}


Car initializeCar() {
Car varCar;

varCar.does_incr = 1;

varCar.speed = 10;

varCar.increment = 1;

varCar.gameObject.x = 0;

varCar.gameObject.y = 0;

varCar.gameObject.width = 0;

varCar.gameObject.height = 0;

varCar.gameObject.color = WHITE;
return varCar;
}




int main(void) {
init();
Car car1;

car1 = initializeCar();

car1.engine1 = initializeEngine();

car1.gameObject.width = 10;
car1.gameObject.y = 200;
car1.gameObject.x = 100;
car1.gameObject.width = 100;
car1.gameObject.height = 10;
while(1) {
ClearOTagR(ot[db], OTLEN);  // Clear ordering table 
if (car1.does_incr) {
if (car1.increment<200) {
car1.increment++;
}

else {
car1.does_incr = 0;
}
}

else {
if (car1.increment>0) {
car1.increment--;
}

else {
car1.does_incr = 1;
}
}

car1.gameObject.x = car1.increment;
renderGameObject(&car1.gameObject);
display();

}
}