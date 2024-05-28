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


typedef struct Platform{
GameObject gameObject;
}Platform;


typedef struct Player{
GameObject gameObject;

int playerY;
}Player;




Platform initializePlatform() {
Platform varPlatform;

varPlatform.gameObject.x = 0;

varPlatform.gameObject.y = 0;

varPlatform.gameObject.width = 0;

varPlatform.gameObject.height = 0;

varPlatform.gameObject.color = WHITE;
return varPlatform;
}


Player initializePlayer() {
Player varPlayer;

varPlayer.playerY = 0;

varPlayer.gameObject.x = 0;

varPlayer.gameObject.y = 0;

varPlayer.gameObject.width = 0;

varPlayer.gameObject.height = 0;

varPlayer.gameObject.color = WHITE;
return varPlayer;
}




int main(void) {
init();
Player player;

Platform platform;

player = initializePlayer();

platform = initializePlatform();

platform.gameObject.x = 0;
platform.gameObject.y = 208;
platform.gameObject.width = 16;
platform.gameObject.height = 16;
player.gameObject.x = 146;
player.gameObject.y = 40;
player.gameObject.width = 16;
player.gameObject.height = 16;
player.gameObject.color = RED;
while(1) {
ClearOTagR(ot[db], OTLEN); 
if (player.gameObject.x==platform.gameObject.x) {
platform.gameObject.x = 50;
platform.gameObject.y = 100;}

if (player.gameObject.x<platform.gameObject.x) {
player.gameObject.x = player.gameObject.x+1;}

if (player.gameObject.x>platform.gameObject.x) {
player.gameObject.x = player.gameObject.x-1;}

if (player.gameObject.y<platform.gameObject.y) {
player.gameObject.y = player.gameObject.y+1;}

if (player.gameObject.y>platform.gameObject.y) {
player.gameObject.y = player.gameObject.y-1;}

renderGameObject(&platform.gameObject);

renderGameObject(&player.gameObject);
display();

}
}