#ifndef SEMA_H
#define SEMA_H

#include "AST.h"

class SematicCheck {
public:
  SematicCheck() {};
  ~SematicCheck() {};
  bool check(AST *Tree);
};

#endif