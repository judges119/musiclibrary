//
//  main.cpp
//  MD5LibraryCheck
//
//  Created by Adam O'Grady on 6/09/2014.
//  Copyright (c) 2014 Adam O'Grady. All rights reserved.
//
//  Compile with g++ main.cpp -lssl -lcrypto -o md5hash
//

#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include <openssl/md5.h>

using namespace std;

int main(int argc, const char * argv[])
{
    FILE *file = fopen(argv[1], "rb");
    unsigned char hash[MD5_DIGEST_LENGTH];
    MD5_CTX md5;
    MD5_Init(&md5);
    const int bufSize = MD5_DIGEST_LENGTH;
    void *buffer = malloc(bufSize);
    size_t bytesRead = 0;
    while ((bytesRead = fread(buffer, 1, bufSize, file))) {
        MD5_Update(&md5, buffer, bytesRead);
    }
    MD5_Final(hash, &md5);
    fclose(file);
    
    stringstream ss;
    for (int i = 0; i < MD5_DIGEST_LENGTH; i++) {
        ss << hex << setw(2) << setfill('0') << (int)hash[i];
    }
    std::cout << ss.str();
    
    free(buffer);
    return 0;
}

