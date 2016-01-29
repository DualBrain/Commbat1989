/* Comm-Bat a two player tank game  by Jim Veneskey 12-18-89 (c)  */



#include <conio.h>
#include <dos.h>
#include <graphics.h>

#define UP 1
#define DOWN 2
#define LEFT 3
#define RIGHT 4

/* All externally accessed variables are defined here */

char bigmap[86][86],twas[9];
int tankx[9],tanky[9],tankm[9];
int decoyx[4],decoyy[4],decoym[4];
int shields[10],tank=1,decoy=1,map=1;
int basex,basey,basem;
int realx,realy;
int zx=0;     /* testing only */


main()
{
    /* Define all internally used variables               */
    
    int loopy,loopx;
    int keyin=0;
    
    
    /* Initialize BigMap array to all blanks     */
    
    for(loopy=1; loopy<=84; loopy++)
        for(loopx=1; loopx<=84; loopx++)
            bigmap[loopy][loopx]=' ';
    
    for(loopy=1; loopy<=2; loopy++)
        for(loopx=1; loopx<=84; loopx++)
	       {
               bigmap[loopy][loopx]='#';
               bigmap[loopy+82][loopx]='#';
           }
    
    for(loopx=1; loopx<=2; loopx++)
        for(loopy=1; loopy<=84; loopy++)
        {
            bigmap[loopy][loopx]='#';
            bigmap[loopy][loopx+82]='#';
        }
    
    
    for(loopx=1; loopx<=8; loopx++)
	   {
           shields[loopx]=100;
           twas[loopx]='B';
       }
    
    /*********** Main programming starts here ***********/
    
    
    clrscr();
    drawscr();
    initgame();
    cursor(1,0);
    views();
    while(keyin != 27)
    {
        keyin = check_keybrd();
        switch(keyin)
        {
            case 172:movetank(UP);break;
            case 180:movetank(DOWN);break;
            case 175:movetank(LEFT);break;
            case 177:movetank(RIGHT);break;
            case 182:clrtext();break;
            case 49:tank=1;views();break;
            case 50:tank=2;views();break;
            case 51:tank=3;views();break;
            case 52:tank=4;views();break;
            case 53:tank=5;views();break;
            case 54:tank=6;views();break;
            case 55:tank=7;views();break;
            case 56:tank=8;views();break;
            case 33:decoy=1;views();break;
            case 64:decoy=2;views();break;
            case 35:decoy=3;views();break;
        }
        remote();
    }
    
    cursor(12,13);     /* Show Cursor */
    return(0);
    
}
/* What follows are function declarations */





/* BOX()   uses upper left and lower right corners as parameters */

box(y1,x1,y2,x2)
int y1,x1,y2,x2;
{
    int temp1;
    for(temp1=x1; temp1<=x2; temp1++)
	   {
           gotoxy(temp1,y1);
           cputs("Õ");
           gotoxy(temp1,y2);
           cputs("Õ");
           
       }
    
    for(temp1=y1; temp1<=y2; temp1++)
	   {
           gotoxy(x1,temp1);
           cputs("∫");
           gotoxy(x2,temp1);
           cputs("∫");
       }
    gotoxy(x1,y1);
    cputs("…");
    gotoxy(x1,y2);
    cputs("»");
    gotoxy(x2,y1);
    cputs("ª");
    gotoxy(x2,y2);
    cputs("º");
    return(0);
    
}

/* a print @ type of routine            */

printat(row,column,text)
int row,column;
char text[40];
{
    gotoxy(column,row);
    cputs(text);
    return(0);
}

/* This routine simply clears out the old messages */
clrtext()
{
    gotoxy(3,24);
    cputs("                                            ");
    return(0);
}

/*  This routine calls other routines to set up the main game screen */

drawscr()
{
    box(1,39,22,80);
    box(16,4,22,10);
    box(16,16,22,22);
    box(16,28,22,34);
    box(1,1,15,38);
    box(23,2,25,79);
    printat(1,53,"[ Main Map ]");
    printat(22,54,"[ Map # 1 ]");
    printat(1,6,"[ Comm-Bat Command Listing ]");
    printat(23,34,"[ Messages ]");
    printat(16,5,"[ D ]");
    printat(16,17,"[ B ]");
    printat(16,29,"[ T ]");
    return(0);
}
/* this routine polls the keyboard to see if a key has been struck.
 if not, it returns a zero.
 if so, it returns a value either 1-127 for normal keys or 201-209 for
 keypad.  non valid keys are not supported and ignored.  */

check_keybrd(c)
int c;
{
    if (! kbhit() ) return (0);  /* if no keystruck return with "0" */
    c = getch();                 /* Otherwise check for normal key  */
    if (c) return (c);           /* if it is normal, return it.     */
    c = getch();                 /* Must be an extended key - again */
    return (c+100);              /* add 100 to act as a flag.       */
    
}
/* this routine is responsible for moving the tank if it is ok to do so */
movetank(direction)
int direction;
{
    int zq;
    
    if(!shields[tank])
    {
        printat(24,3,"\x7");
        printat(24,3,"That Tank is dead - immobile!           ");
        return(0);
    }
    
    erasetank(tank);
    
    zq=badmove(direction);  /* 0 = Normal, 1 =  RIP, 2 = New Map   */
    
    if(zq == 1)
    {
        shields[tank]=0;
        printat(24,3,"Tank has hit map edge and exploded!");
        printat(24,3,"\x7");
        convert(tanky[tank],tankx[tank],tankm[tank]);
        if((bigmap[realy][realx] !='B') && (bigmap[realy][realx] != 'D'))
        {
            bigmap[realy][realx] = 'X';
            if(tankm[tank] == map)
            {
                gotoxy(tankx[tank]+39,tanky[tank]+1);
                cputs("X");
            }
        }
        views();
        return(0);
    }
    
    if(zq == 2)
    {
        convert(tanky[tank],tankx[tank],tankm[tank]);
        twas[tank]=bigmap[realy] [realx];
        if((twas[tank] !='B') && (twas[tank] !='D'))
            bigmap[realy] [realx] = 48+tank;
        views();
        return(0);
    }
    
    switch(direction)
    {
        case UP:tanky[tank]-=1;break;
        case DOWN:tanky[tank]+=1;break;
        case LEFT:tankx[tank]-=1;break;
        case RIGHT:tankx[tank]+=1;break;
    }
    
    update(tank);
    views();
    return(0);
}

/* This function tests the legality of a move - if it will hit a wall. */
badmove(direction)
int direction;
{
    if(direction == LEFT  && tankx[tank] > 1)
        return(0);  /* Tank was not on the edge no further test */
    else
        if(direction == LEFT && (! (tankm[tank] % 2)))
        {
            tankx[tank]=40;
            tankm[tank]=tankm[tank]-1;
            return(2); /* Tank was on edge but EVEN map - adjust and ok */
        }
        else
            if(direction == LEFT)
                return(1); /* Tank was on edge of an ODD map - bad news!  */
    
    if(direction == RIGHT && tankx[tank] < 40)
        return(0);  /* Tank was not on edge.  */
    else
        if(direction == RIGHT && (tankm[tank] % 2))
        {
            tankx[tank]=1;
            tankm[tank]=tankm[tank]+1;
            return(2); /* Tank was on ODD map edge - adjust and ok */
        }
        else
            if(direction == RIGHT)
                return(1);  /* Tank was on edge of EVEN map - bad news! */
    
    if(direction == UP && tanky[tank] > 1)
        return(0);  /* Tank was not at edge.  */
    else
        if(direction == UP && tankm[tank] > 2)
        {
            tanky[tank]=20;
            tankm[tank]=tankm[tank]-2;
            return(2);  /* tank rose a map - adjust and ok */
        }
        else
            if(direction == UP)
                return(1); /* Tank tried to go off top of map */
    
    if(direction == DOWN && tanky[tank] < 20)
        return(0);  /* Tank was not at edge. */
    else
        if(direction == DOWN && tankm[tank] < 7)
        {
            tanky[tank]=1;
            tankm[tank]=tankm[tank]+2;
            return(2);  /* tank descended a map */
        }
        else
            if(direction == DOWN)
                return(1); /* Tank was on bottom.... */
    
    return(0); /* this line should not be needed logically */
}

/*  UpdateTank(tank #) - updates display of tanks moves   */
update(tank)
int tank;
{
    convert(tanky[tank],tankx[tank],tankm[tank]);
    twas[tank]=bigmap[realy] [realx];
    if((twas[tank] !='B') && (twas[tank] != 'D'))
        bigmap[realy] [realx] = 48+tank;
    gotoxy(10,6);
    printf("Tank X is %d ",tankx[tank]);
    gotoxy(10,7);
    printf("Tank Y is %d ",tanky[tank]);
    gotoxy(10,8);
    printf("Tank M is %d",tankm[tank]);
    gotoxy(10,9);
    printf("Contents of TWAS is %c",twas[tank]);
    gotoxy(10,10);
    printf("Tank # = %d",tank);
    return(0);
    
}
/*  ERASETANK(tank #) - removes old image of tanks from screen */
erasetank(tank)
int tank;
{
    convert(tanky[tank],tankx[tank],tankm[tank]);
    bigmap[realy] [realx]=twas[tank];
    if(tankm[tank] == map)
    {
        gotoxy(tankx[tank]+39,tanky[tank]+1);
        printf("%c",twas[tank]);
    }
    return(0);
}
/* ShowTank(tank #) - Displays currently selected tank IF on current map */
showtank(tank)
int tank;
{
    int a,b,c,d;
    if(tankm[tank] == map)
    {
        if(tankx[tank]==1)
            a=0;
        else if(tankx[tank]==2)
            a=1;
        else a=2;
        
        if(tankx[tank]==40)
            c=0;
        else if(tankx[tank]==39)
            c=1;
        else c=2;
        
        if(tanky[tank]==1)
            b=0;
        else if(tanky[tank]==2)
            b=1;
        else b=2;
        
        if(tanky[tank]==20)
            d=0;
        else if(tanky[tank]==19)
            d=1;
        else d=2;
        
        movetext(31-a,19-b,31+c,19+d,tankx[tank]+39-a,tanky[tank]+1-b);
        
        
    }
    return(0);
}

/* Cursor - adjusts the size of cursor in scan lines */
cursor(a,b)
char a,b;
{
    union REGS r;
    r.h.ah = 1;
    r.h.ch = a;
    r.h.cl = b;
    int86(0x10,&r,&r);
    return(0);
}

/* Remote - this simulates tasking in background */
remote()
{
    zx=zx+1;
    if (zx==100)
        zx=0;
    
    gotoxy(10,11);
    printf("Count is %d and counting.",zx);
    return(0);
}
/* Initialize game - get base co-ordinates  */
initgame()
{
    int loop;
    gotoxy(2,5);
    printf("Enter base co-ords X,Y,Map ");
    scanf("%d,%d,%d",&basex,&basey,&basem);
    convert(basey,basex,basem);
    bigmap[realy][realx]='B';
    for(loop=1; loop<=8; loop++)
    {
        tankx[loop]=basex;
        tanky[loop]=basey;
        tankm[loop]=basem;
        if(loop<4)
        {
            decoyx[loop]=basex+(4*(loop-1))+2;
            decoyy[loop]=basey+(4*(loop-1))+2;
            decoym[loop]=basem;
            convert(decoyy[loop],decoyx[loop],decoym[loop]);
            bigmap[realy][realx]='D';
        }
        
    }
    return(0);
}
/* Update Tank viewport                     */
tankview(tank)
int tank;
{
    int x,y;
    convert(tanky[tank],tankx[tank],tankm[tank]);
    for(x=0; x<=4; x++)
        for(y=0; y<=4; y++)
        {
            gotoxy(29+x,17+y);
            printf("%c",bigmap[realy-2+y] [realx-2+x]);
        }
    showtank(tank);
    return(0);
}

/* Update Base viewport */
baseview()
{
    int x,y;
    convert(basey,basex,basem);
    for(x=0; x<=4; x++)
        for(y=0; y<=4; y++)
        {
            gotoxy(17+x,17+y);
            printf("%c",bigmap[realy-2+y][realx-2+x]);
        }
    return(0);
}

/* Update Decoy viewport */
decoyview(decoy)
int decoy;
{
    int x,y;
    convert(decoyy[decoy],decoyx[decoy],decoym[decoy]);
    for(x=0; x<=4; x++)
        for(y=0; y<=4; y++)
        {
            gotoxy(5+x,17+y);
            printf("%c",bigmap[realy-2+y][realx-2+x]);
        }
    return(0);
}

/* Convert Tank Psuedo co-ords into actual BigMap Co-ords */
convert(y,x,map)
int y,x,map;
{
    if(map % 2)
        realx = x+2;
        else realx = x+42;
            realy=y+2+((map+1)/2-1)*20;
            return(0);
}

/* Package all view handling nice and neat */
views()
{
    tankview(tank);
    baseview();
    decoyview(decoy);
    return(0);
}
