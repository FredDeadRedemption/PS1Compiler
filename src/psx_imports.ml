let generate_psx_code =
  "#include <sys/types.h>\n" ^
  "#include <libgte.h>\n" ^
  "#include <libetc.h>\n" ^
  "#include <libgpu.h>\n" ^
  "#include <libapi.h>\n" ^
  "#include <stdlib.h>\n\n" ^
  "#define OTLEN 8\n" ^
  "#define MAX_SNAKE_LENGTH 300\n" ^
  "DISPENV disp[2];\n" ^
  "DRAWENV draw[2];\n" ^
  "int db = 0;\n" ^
  "u_long ot[2][OTLEN];\n" ^
  "char pribuff[2][32768];\n" ^
  "char *nextpri;\n" ^
  "TILE *tile;\n" ^
  "#define PAD_SELECT      1\n" ^
  "#define PAD_L3          2\n" ^
  "#define PAD_R3          4\n" ^
  "#define PAD_START       8\n" ^
  "#define PAD_UP          16\n" ^
  "#define PAD_RIGHT       32\n" ^
  "#define PAD_DOWN        64\n" ^
  "#define PAD_LEFT        128\n" ^
  "#define PAD_L2          256\n" ^
  "#define PAD_R2          512\n" ^
  "#define PAD_L1          1024\n" ^
  "#define PAD_R1          2048\n" ^
  "#define PAD_TRIANGLE    4096\n" ^
  "#define PAD_CIRCLE      8192\n" ^
  "#define PAD_CROSS       16384\n" ^
  "#define PAD_SQUARE      32768\n" ^
  "typedef struct _PADTYPE\n" ^
  "{\n" ^
  "   unsigned char	stat;\n" ^
  "   unsigned char	len:4;\n" ^
  "   unsigned char	type:4;\n" ^
  "   unsigned short	btn;\n" ^
  "   unsigned char	rs_x,rs_y;\n" ^
  "   unsigned char	ls_x,ls_y;\n" ^
  "} PADTYPE;\n" ^
  "u_char padbuff[2][34];\n" ^
  "void init(void) {\n" ^
  "   ResetGraph(0);\n" ^
  "   SetDefDispEnv(&disp[0], 0, 0, 320, 240);\n" ^
  "   SetDefDispEnv(&disp[1], 0, 240, 320, 240);\n" ^
  "   SetDefDrawEnv(&draw[0], 0, 240, 320, 240);\n" ^
  "   SetDefDrawEnv(&draw[1], 0, 0, 320, 240);\n" ^
  "   setRGB0(&draw[0], 63, 0, 127);\n" ^
  "   setRGB0(&draw[1], 63, 0, 127);\n" ^
  "   draw[0].isbg = 1;\n" ^
  "   draw[1].isbg = 1;\n" ^
  "   PutDispEnv(&disp[0]);\n" ^
  "   PutDrawEnv(&draw[0]);\n" ^
  "   nextpri = pribuff[0];\n" ^
  "   InitPAD( padbuff[0], 34, padbuff[1], 34 );\n" ^
  "   StartPAD();\n" ^
  "   ChangeClearPAD( 1 );\n" ^
  "   FntLoad(960, 0);\n" ^
  "   FntOpen(0, 16, 320, 224, 0, 100);\n" ^
  "}\n" ^
  "void display(void) {\n" ^
  "   DrawSync(0);\n" ^
  "   VSync(0);\n" ^
  "   PutDispEnv(&disp[db]);\n" ^
  "   PutDrawEnv(&draw[db]);\n" ^
  "   SetDispMask(1);\n" ^
  "   DrawOTag(ot[db]+OTLEN-1);\n" ^
  "   db = !db;\n" ^
  "   nextpri = pribuff[db];\n" ^
  "}\n" ^
  "typedef struct Position {\n" ^
  "   int x;\n" ^
  "   int y;\n" ^
  "} Position;\n" ^
  "typedef struct TextureSize {\n" ^
  "   int width;\n" ^
  "   int height;\n" ^
  "} TextureSize;\n" ^
  "typedef struct Color{\n" ^
  "   int r;\n" ^
  "   int g;\n" ^
  "   int b;\n" ^
  "} Color;\n" ^
  "typedef struct GameObject {\n" ^
  "   Position position;\n" ^
  "   TextureSize textureSize;\n" ^
  "   Color color;\n" ^
  "} GameObject;\n" ^
  "color_t RED = {255, 0, 0};\n" ^
  "color_t BLUE = {0, 0, 255};\n" ^
  "color_t GREEN = {0, 255, 0};\n" ^
  "color_t YELLOW = {255, 255, 0};\n" ^
  "color_t WHITE = {255, 255, 255};\n" ^
  "void renderGameObject(gameObject_t* object) {\n" ^
  "  tile = (TILE*)nextpri;      // Cast next primitive\n" ^
  "  setTile(tile);              // Initialize the primitive (very important)\n" ^
  "  setXY0(tile, object->position.x, object->position.y);       // Set primitive (x,y) position\n" ^
  "  setWH(tile, object->textureSize.width, object->textureSize.height);        // Set primitive size\n" ^
  "  setRGB0(tile, object->color.r, object->color.g, object->color.b); // Set color yellow\n" ^
  "  addPrim(ot[db], tile);      // Add primitive to the ordering table\n" ^
  "  nextpri += sizeof(TILE);    // Advance the next primitive pointer\n" ^
  "}\n"
