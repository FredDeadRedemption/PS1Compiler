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
typedef struct Position {
   int x;
   int y;
} Position;
typedef struct TextureSize {
   int width;
   int height;
} TextureSize;
typedef struct Color{
   int r;
   int g;
   int b;
} Color;
typedef struct GameObject {
   Position position;
   TextureSize textureSize;
   Color color;
} GameObject;
color_t RED = {255, 0, 0};
color_t BLUE = {0, 0, 255};
color_t GREEN = {0, 255, 0};
color_t YELLOW = {255, 255, 0};
color_t WHITE = {255, 255, 255};
void renderGameObject(gameObject_t* object) {
  tile = (TILE*)nextpri;      // Cast next primitive
  setTile(tile);              // Initialize the primitive (very important)
  setXY0(tile, object->position.x, object->position.y);       // Set primitive (x,y) position
  setWH(tile, object->textureSize.width, object->textureSize.height);        // Set primitive size
  setRGB0(tile, object->color.r, object->color.g, object->color.b); // Set color yellow
  addPrim(ot[db], tile);      // Add primitive to the ordering table
  nextpri += sizeof(TILE);    // Advance the next primitive pointer
}


typedef struct Fucker{
GameObject gameobject;
}Fucker;




Fucker initializeFucker() {
Fucker varFucker;
return varFucker;
}




int main(void) {
int x = 10;

x++;

while(1) {

}
}