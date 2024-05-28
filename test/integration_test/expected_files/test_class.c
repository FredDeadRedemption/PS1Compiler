typedef struct Tetrahedron{
GameObject gameObject;

int edges;

int faces;

int rotation;
}Tetrahedron;


void spin(Tetrahedron* varTetrahedron, int speed);

Tetrahedron initializeTetrahedron() {
Tetrahedron varTetrahedron;

varTetrahedron.edges = 6;

varTetrahedron.faces = 4;

varTetrahedron.gameObject.x = 0;

varTetrahedron.gameObject.y = 0;

varTetrahedron.gameObject.width = 0;

varTetrahedron.gameObject.height = 0;

varTetrahedron.gameObject.color = WHITE;
return varTetrahedron;
}


void spin (Tetrahedron* varTetrahedron, int speed) {
varTetrahedron->rotation = speed;

}


int main(void) {
init();
Tetrahedron theBestShape;

theBestShape = initializeTetrahedron();

spin(&theBestShape, 1);
int initialspeed = theBestShape.rotation;

while(1) {
ClearOTagR(ot[db], OTLEN); 
renderGameObject(&theBestShape.gameObject);
display();

}
}