
g++ -o acyclic acyclic.cc -lOGDF

g++ -o manual manual.cc -lOGDF

g++ -o hierarchy hierarchy.cc  -lOGDF -lCOIN -lpthread

g++ -o hierarchy-predefined hierarchy-predefined.cc  -lOGDF -lCOIN -lpthread
