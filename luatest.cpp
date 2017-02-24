#include <stdio.h>

extern "C" {
    #include "lua.h"
    #include "lualib.h"
    #include "luaxlib.h"
}

lua_State* L;

int luaadd ( int x, int y)
{
    int sum;
    lua_getglobal(L, "add"); // getting the add function from Lua !!!!
    lua_pushnumber(L, x);
    lua_pushnumber(L, y);
    lua_call(L, 2, 1);
    sum = (int)lua_tointeger(L, -1);
    lua_pop(L, 1);
    return sum;
}

int main ( int argc, char *argv[] )
{
    int sum;
    L = lua_open();
    luaL_openlibs(L);
    luaL_dofile(L, "temp"); // load lua chunk defining "add" !!!!
    sum = luaadd( 10, 15 );
    printf( "The sum is %d\n", sum );
    lua_close(L);
    printf( "Press enter to continue ... " );
    getchar();
    return 0;
}