
typedef enum nodeType{
    factorN,
    simpExpressionN,
    termN,
    expressionN,
    wrIntN,
    whileStmtN,
    elseN,
    ifstmtN,
    assnmtN,
    stmtN,
    stmtSeqN,
    typeN,
    declN,
    ProgramN
} NodeType;

typedef struct typeNode TypeNode;
typedef struct factorNode FactorNode;
typedef struct termNode TermNode;
typedef struct simpExpressionNode SimpExpressionNode;
typedef struct expressionNode ExpressionNode;
typedef struct wrIntNode WrIntNode;
typedef struct stmtSeqNode StmtSeqNode;
typedef struct whileStmtNode WhileStmtNode;
typedef struct elseNode ElseNode;
typedef struct ifstmtNode IfstmtNode;
typedef struct assnmtNode AssnmtNode;
typedef struct stmtNode StmtNode;
typedef struct declNode DeclNode;
typedef struct programNode ProgramNode;

typedef struct node Node;

struct node{
    NodeType type;
    void* nodePtr;
};

struct typeNode{
    int subtype;
    //0 int
    //1 bool
};

struct factorNode
{
    int subtype;
    int num; //1
    char* boollit; //2
    char* identifier; //0
    Node* expr; //3
};

struct termNode{
    int subtype;
    char* op2;
    Node* fac1;
    Node* fac2;
};

struct simpExpressionNode{
    int subtype;
    char* op3;
    Node* term1;
    Node* term2;
};

struct expressionNode{
    int subtype;
    char* op4;
    Node* sExp1;
    Node* sExp2;  
};

struct wrIntNode{
    Node* expr;
};

struct stmtSeqNode{
    int subtype;
    Node* stmt;
    Node* stmtseq; 
};

struct whileStmtNode{
    Node* expr;
    Node* stmtseq;
};

struct elseNode{
    int subtype;
    Node* stmtseq;
};

struct ifstmtNode{
    Node* expr;
    Node* stmtseq;
    Node* elsestmt; 
};

struct assnmtNode{
    int subtype;
    Node* expr;
    char* identifier;
};

struct stmtNode{
    int subtype;
    Node* assmt; //0
    Node* ifstmt; //1
    Node* whileStmt; //2
    Node* wrInt; //3
};

struct declNode{
    int subtype;
    char* identifier;
    Node* typeN;
    Node* decls;
};

struct programNode{
    Node* decl;
    Node* stmtseq;
};






