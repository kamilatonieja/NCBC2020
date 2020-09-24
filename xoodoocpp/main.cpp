/*
Implementation by Ronny Van Keer, hereby denoted as "the implementer".

For more information, feedback or questions, please refer to our website:
https://keccak.team/

To the extent possible under law, the implementer has waived all copyright
and related or neighboring rights to the source code in this file.
http://creativecommons.org/publicdomain/zero/1.0/
*/


#include "api.h"
#include "Xoodyak.h"
#include <string.h>


int main() {

    Xoodoo *xoodoo = new Xoodoo(384, 1);


    UINT8 *word1 = (UINT8 *) "123456789012345678901234567890123456789012345678";
    UINT8 *word = (UINT8 *)"3";

    //XoodooState *xoodooState = new XoodooState(word);

    std::cout << "" << std::endl;
    std::cout << "" << std::endl;
    xoodoo->setLog(LOG_FINAL,std::cout);
    xoodoo->operator()(word);
    //xoodoo->stepIota(*(new XoodooState()),-1);

    return 0;
}
