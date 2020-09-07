/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    typedef struct symbolTable{
    int Index;
    char Name[20];
    char Type[20];
    int Address;
    int Lineno;
    char Element_type[20];
    struct symbolTable* next;
    }symbolTable;

    typedef struct table{
        symbolTable* head;
        symbolTable* tail;
        struct table* next;
        struct table* prev;
        int scope_index;
    }table;

    table* current=NULL;

    /* Symbol table function - you can add new function if needed. */
    static void create_symbol();
    static void insert_symbol(char* Name,char* Type,int Lineno,char* Element_type);
    static symbolTable* lookup_symbol(char* Name);
    static void dump_symbol();

    int scope=-1;
    int address=0;
    int LIT_Type;
    int ifINT=0;
    int ifADD_ASIGN=0;
    int ifID=0;
    int beforeINT=0;
    int ok=0;

    FILE *file;

    int label_index=0;
    int isARRAY=0;
    int ifASSIGN=0;
    int storeADDRESS=0;
    int for_index=0;
    int if_index=0;
    int enter_if=0;
    int else_index=0;
    int for_count=0;
    int for_num=0;
    
    
    /*Global variables */
    int HAS_ERROR = 0;

%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    /* ... */
}

/* Token without return */
%token VAR
%token INC DEC
%token NEWLINE
%token PRINT PRINTLN IF ELSE 
%token TRUE FALSE

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <s_val> identifier
%token <s_val> FOR
%token <s_val> INT FLOAT BOOL STRING
%token <s_val> ADD SUB MUL QUO REM
%token <s_val> GTR LSS
%token <s_val> GEQ LEQ EQL NEQ
%token <s_val> NOT
%token <s_val> LAND LOR
%token <s_val> ASSIGN
%token <s_val> ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN

/* Nonterminal with return, which need to sepcify type */
/*%type <type> Type TypeName ArrayType*/
%type <s_val> Type TypeName
%type <s_val> cmp_op add_op mul_op binary_op highFOR highIF highELSE 
%type <s_val> assign_op
%type <i_val> Operand
%type <i_val> IndexExpr Literal
%type <i_val> identifierStmt
%type <i_val> Expression
%type <s_val> ForStmt

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList {dump_symbol();}
;

StatementList
    : Statement StatementList
    | Statement
;

Statement 
    : DeclarationStmt NEWLINE 
    | SimpleStmt NEWLINE 
    | PrintStmt NEWLINE
    | CompoundStmt 
    | AssignmentStmt NEWLINE
    | NEWLINE
    | IfStmt NEWLINE
    | ForStmt NEWLINE
    | Condition NEWLINE
;

ForStmt  
    : highFOR Expression {
                        HAS_ERROR=1;
                        printf("error:%d: non-bool (type ", yylineno+1); 
                        if($2!=0)
                        {
                            if($2==1){
                                printf("int32");}
                                else if($2==2){printf("float32");}printf(") used as for condition\n");}
                        } Block
    | highFOR Condition{
                        fprintf(file,"ifeq L_for_exit");
                        fprintf(file,"%d%d\n",for_count-1,for_index-1);
                        fprintf(file,"L_for_block");
                        fprintf(file,"%d%d :\n",for_count-1,for_index-1);
    
    } Block {
        fprintf(file,"goto L_for_begin");
        fprintf(file,"%d%d\n",for_count-1,for_index-1);
        fprintf(file,"L_for_exit");
        fprintf(file,"%d%d :\n",for_count-1,for_index-1);
    }
    | highFOR ForClause Block {
        if(for_index!=0)
            {
                fprintf(file,"goto L_for_post%d%d\n",for_count-1,for_index-1);
                fprintf(file,"L_for_exit%d%d :\n",for_count-1,for_index-1);
                
                for_count-=1;

                for_index-=1;
            }
    }
;

highFOR
    : FOR {
        for_num+=1;
        for_index+=1;
        for_count=for_num;
        fprintf(file,"L_for_begin");
        fprintf(file,"%d%d :\n",for_count-1,for_index-1);
    }
ForClause 
    : AssignmentStmt ';' {
        fprintf(file,"L_for_condition%d%d :\n",for_count-1,for_index-1);
    } Condition{
                        fprintf(file,"ifeq L_for_exit");
                        fprintf(file,"%d%d\n",for_count-1,for_index-1);
                        fprintf(file,"goto L_for_block");
                        fprintf(file,"%d%d\n",for_count-1,for_index-1);
    } 
    ';' {
        fprintf(file,"L_for_post%d%d :\n",for_count-1,for_index-1);
    }IncDecStmt{
        fprintf(file,"goto L_for_condition%d%d\n",for_count-1,for_index-1);
        fprintf(file,"L_for_block%d%d :\n",for_count-1,for_index-1);
    }
;

IfStmt 
    : highIF Condition Block {
        //fprintf(file,"goto L_if");
        //fprintf(file,"%d_exit\n",if_index-1);
        fprintf(file,"L_if");
        fprintf(file,"%d_false :\n",if_index-1);
        fprintf(file,"L_if");
        fprintf(file,"%d_exit :\n",if_index-1);
        //if_index+=1;
        
    }
    | highIF Expression {
                        HAS_ERROR=1;
                        printf("error:%d: non-bool (type ", yylineno+1); 
                        if($2!=0)
                        {
                            if($2==1){
                                printf("int32");}
                                else if($2==2){printf("float32");}printf(") used as for condition\n");}} Block 
    | highIF Condition Block highELSE Block{fprintf(file,"L_if%d_exit :\n",if_index-1);}
    | highIF Condition Block highELSE IfStmt {
        for(int i=0;i<else_index;i++)
        {
            fprintf(file,"L_if%d_exit:\n",if_index-1);
            if_index-=1;
        }
    }
;



highIF
    : IF { 
                enter_if=1;
                if_index+=1;
            }

highELSE
    : ELSE {
        fprintf(file,"L_if");
        fprintf(file,"%d_false :\n",if_index-1);
        else_index+=1;
    }
Block  
    : left_block StatementList right_block
;

AssignmentStmt
    : Expression  assign_op Expression {  
                                    if(ifINT==1 && beforeINT==1 && ifADD_ASIGN==1 && ifID==1 && ok==0)
                                    {
                                        HAS_ERROR=1;
                                        printf("error:%d: cannot assign to ",yylineno);
                                                if($1==1)
                                                {
                                                    printf("int32");
                                                }
                                                else if($1==2)
                                                {
                                                    printf("float32");
                                                }
                                                else if($1==3)
                                                {
                                                    printf("string");
                                                }
                                                else if($1==4)
                                                {
                                                    printf("bool");
                                                }
                                                printf("\n");
                                        
                                    }
                                    if($1!=0)
                                    {
                                        if($1!=$3)
                                        {
                                            HAS_ERROR=1;
                                            printf("error:%d: invalid operation: %s (mismatched types ",yylineno,$2);
                                                if($1==1)
                                                {
                                                    printf("int32");
                                                }
                                                else if($1==2)
                                                {
                                                    printf("float32");
                                                }
                                                else if($1==3)
                                                {
                                                    printf("string");
                                                }
                                                else if($1==4)
                                                {
                                                    printf("bool");
                                                }
                                                printf(" and ");
                                                if($3==1)
                                                {
                                                    printf("int32)\n");
                                                }
                                                else if($3==2)
                                                {
                                                    printf("float32)\n");
                                                }
                                                else if($3==3)
                                                {
                                                    printf("string)\n");
                                                }
                                                else if($3==4)
                                                {
                                                    printf("bool)\n");
                                                }
                                        }

                                        
                                        
                                    }
                                    
                                    printf("%s\n",$2);

                                    if(strcmp($2,"ADD_ASSIGN")==0)
                                    {
                                        if($1==1)
                                        {
                                            fprintf(file,"iadd\n");
                                        }
                                        else if($1==2)
                                        {
                                            fprintf(file,"fadd\n");
                                        }
                                    }
                                    else if(strcmp($2,"SUB_ASSIGN")==0)
                                    {
                                        if($1==1)
                                        {
                                            fprintf(file,"isub\n");
                                        }
                                        else if($1==2)
                                        {
                                            fprintf(file,"fsub\n");
                                        }
                                    }
                                    else if(strcmp($2,"MUL_ASSIGN")==0)
                                    {
                                        if($1==1)
                                        {
                                            fprintf(file,"imul\n");
                                        }
                                        else if($1==2)
                                        {
                                            fprintf(file,"fmul\n");
                                        }
                                    }
                                    else if(strcmp($2,"QUO_ASSIGN")==0)
                                    {
                                        if($1==1)
                                        {
                                            fprintf(file,"idiv\n");
                                        }
                                        else if($1==2)
                                        {
                                            fprintf(file,"fdiv\n");
                                        }
                                    }
                                    else if(strcmp($2,"REM_ASSIGN")==0)
                                    {
                                        if($1==1)
                                        {
                                            fprintf(file,"irem\n");
                                        }
                                        else if($1==2)
                                        {
                                            fprintf(file,"frem\n");
                                        }
                                    }

                                    if(isARRAY==1)
                                    {
                                        if($3==1)
                                        {
                                            fprintf(file,"iastore\n");
                                        }
                                        else if($3==2)
                                        {
                                            fprintf(file,"fastore\n");
                                        }
                                    }
                                    else
                                    {
                                        if($1==1)
                                        {
                                            fprintf(file,"istore %d\n",storeADDRESS);
                                        }
                                        else if($1==2)
                                        {
                                            fprintf(file,"fstore %d\n",storeADDRESS);
                                        }
                                        else if($3==3)
                                        {
                                            fprintf(file,"astore %d\n",storeADDRESS);
                                        }
                                        
                                    }
                                    
                                    ifINT=0;
                                    ifADD_ASIGN=0;
                                    ifID=0;
                                    beforeINT=0;
                                    ok=0;

                                    ifASSIGN=0;
                                    isARRAY=0;
                                }
    | Expression assign_op BOOL_LIT {  
        fprintf(file,"istore %d\n",storeADDRESS);
        printf("%s\n",$2);
    
    }
;

IndexExpr
    : identifier {
        printf("IDENT (name=%s, address=%d)\n",$1,lookup_symbol($1)->Address);
        }'[' {fprintf(file,"aload %d\n",lookup_symbol($1)->Address);} Expression ']' {
                    if(strcmp(lookup_symbol($1)->Element_type,"int32")==0)
                    {
                        $$=1;
                    }
                    else if(strcmp(lookup_symbol($1)->Element_type,"float32")==0)
                    {
                        $$=2;
                    }
                    else if(strcmp(lookup_symbol($1)->Element_type,"string")==0)
                    {
                        $$=3;
                    }
                    else if(strcmp(lookup_symbol($1)->Element_type,"bool")==0)
                    {
                        $$=4;
                    }

                    isARRAY=1;

                    if(ifASSIGN==1)
                    {
                        if(strcmp(lookup_symbol($1)->Element_type,"int32")==0)
                        {
                            fprintf(file,"iaload\n");
                        }
                        else if(strcmp(lookup_symbol($1)->Element_type,"float32")==0)
                        {
                            fprintf(file,"faload\n");
                        }
                    }
                }
;

CompoundStmt
    : left_block StatementList right_block NEWLINE
    | left_block block_item CompoundStmt block_item right_block NEWLINE
;

block_item 
	: DeclarationStmt NEWLINE
;

left_block
    : '{' NEWLINE {create_symbol();}
;

right_block
    : '}'  {dump_symbol();
            if(enter_if!=0)
            {
                fprintf(file,"goto L_if");
                fprintf(file,"%d_exit\n",if_index-1);
                enter_if=0;
            }
            

    }
;

SimpleStmt
    : Expression
    | IncDecStmt
;

Type 
   : TypeName {$$=$1;}
 /*  | ArrayType*/
;


/*ArrayType 
    : '[' Expression ']' Type 
;*/

TypeName
    : INT {$$=$1;}
    | FLOAT {$$=$1;}
    | STRING {$$=$1;}
    | BOOL {$$=$1;}
;

Expression
    : Operand ADD Expression { 
                                if($1!=0 && $3!=0)
                                {
                                    if($1!=$3)
                                    {
                                        HAS_ERROR=1;   
                                        printf("error:%d: invalid operation: %s (mismatched types ",yylineno,$2);
                                        if($1==1)
                                        {
                                            printf("int32");
                                        }
                                        else if($1==2)
                                        {
                                            printf("float32");
                                        }
                                        else if($1==3)
                                        {
                                            printf("string");
                                        }
                                        else if($1==4)
                                        {
                                            printf("bool");
                                        }
                                        printf(" and ");
                                        if($3==1)
                                        {
                                            printf("int32)\n");
                                        }
                                        else if($3==2)
                                        {
                                            printf("float32)\n");
                                        }
                                        else if($3==3)
                                        {
                                            printf("string)\n");
                                        }
                                        else if($3==4)
                                        {
                                            printf("bool)\n");
                                        }

                                    }
                                
                                
                                    printf("%s\n",$2);
                                    if($3==1)
                                    {
                                        fprintf(file,"iadd\n");
                                        
                                    }
                                    else if($3==2)
                                    {
                                        fprintf(file,"fadd\n");
                                    }
                                }    
                            }
    | Operand SUB Expression {  
                                if($1!=0 && $3!=0)
                                {
                                    if($1!=$3)
                                    {
                                        HAS_ERROR=1;
                                        printf("error:%d: invalid operation: %s (mismatched types ",yylineno,$2);
                                        if($1==1)
                                        {
                                            printf("int32");
                                        }
                                        else if($1==2)
                                        {
                                            printf("float32");
                                        }
                                        else if($1==3)
                                        {
                                            printf("string");
                                        }
                                        else if($1==4)
                                        {
                                            printf("bool");
                                        }
                                        printf(" and ");
                                        if($3==1)
                                        {
                                            printf("int32)\n");
                                        }
                                        else if($3==2)
                                        {
                                            printf("float32)\n");
                                        }
                                        else if($3==3)
                                        {
                                            printf("string)\n");
                                        }
                                        else if($3==4)
                                        {
                                            printf("bool)\n");
                                        }

                                    }
                                    printf("%s\n",$2);
                                    if($3==1)
                                    {
                                        fprintf(file,"isub\n");
                                        
                                    }
                                    else if($3==2)
                                    {
                                        fprintf(file,"fsub\n");
                                    }
                                }
                            }
    | Operand REM Expression {  
                                if($1!=0 && $3!=0)
                                {
                                    if($1==2 || $3==2)
                                    {
                                        HAS_ERROR=1;
                                        printf("error:\%d: invalid operation: (operator REM not defined on float32)\n",yylineno);
                                    }
                                    printf("%s\n",$2);
                                    if($1==1)
                                    {
                                        fprintf(file,"irem\n");
                                        
                                    }
                                    else if($1==2)
                                    {
                                        fprintf(file,"frem\n");
                                    }
                                }
                            } 
    | Operand {$$=$1;}
;

Operand 
    : Literal {$$=$1;}
    | identifierStmt {$$=$1;}
    | '(' Operand ')' {$$=$2;}
    | Operand MUL Operand {
                                $$=$1;
                                printf("%s\n", $2);
                                if($1==1)
                                {
                                    fprintf(file,"imul\n");
                                        
                                }
                                else if($1==2)
                                {
                                    fprintf(file,"fmul\n");
                                }
                          }
    | Operand QUO Operand {
                                $$=$1;
                                printf("%s\n", $2);
                                if($1==1)
                                {
                                    fprintf(file,"idiv\n");
                                        
                                }
                                else if($1==2)
                                {
                                    fprintf(file,"fdiv\n");
                                }
                          }
    | Literal ADD Operand {if($1!=$3)
                                {
                                    if($1!=0)
                                    {
                                        HAS_ERROR=1;
                                        printf("error:%d: invalid operation: %s (mismatched types ",yylineno,$2);
                                        if($1==1)
                                        {
                                            printf("int32");
                                        }
                                        else if($1==2)
                                        {
                                            printf("float32");
                                        }
                                        else if($1==3)
                                        {
                                            printf("string");
                                        }
                                        else if($1==4)
                                        {
                                            printf("bool");
                                        }
                                        printf(" and ");
                                        if($3==1)
                                        {
                                            printf("int32)\n");
                                        }
                                        else if($3==2)
                                        {
                                            printf("float32)\n");
                                        }
                                        else if($3==3)
                                        {
                                            printf("string)\n");
                                        }
                                        else if($3==4)
                                        {
                                            printf("bool)\n");
                                        }
                                    }
                                    
                                }
        $$=$1;
        printf("%s\n", $2);
        if($$==1)
        {
            fprintf(file,"iadd\n");
        }
        else if($$==2)
        {
            fprintf(file,"fadd\n");
        }
    }
    | Literal SUB Operand {
                            $$=$1;
                            printf("%s\n", $2);
                            if($$==1)
                            {
                                fprintf(file,"isub\n");
                            }
                            else if($$==2)
                            {
                                fprintf(file,"fsub\n");
                            }
                        }
    | IndexExpr {$$=$1;}
;

identifierStmt
    : identifier {
                    ifID=1;
                    if(lookup_symbol($1)==NULL)
                    {
                        HAS_ERROR=1;
                        printf("error:%d: undefined: %s\n",yylineno+1,$1);
                        $$=0;
                    }
                    else
                    {
                        if(strcmp(lookup_symbol($1)->Type,"int32")==0)
                        {
                            $$=1;
                        }
                        else if(strcmp(lookup_symbol($1)->Type,"float32")==0)
                        {
                            $$=2;
                        }
                        else if(strcmp(lookup_symbol($1)->Type,"string")==0)
                        {
                            $$=3;
                        }
                        else if(strcmp(lookup_symbol($1)->Type,"bool")==0)
                        {
                            $$=4;
                        }
                        
                        printf("IDENT (name=%s, address=%d)\n",$1,lookup_symbol($1)->Address);
                        if(strcmp(lookup_symbol($1)->Type,"int32")==0)
                        {
                            fprintf(file,"iload ");
                            fprintf(file,"%d\n",lookup_symbol($1)->Address);
                        }
                        else if(strcmp(lookup_symbol($1)->Type,"float32")==0)
                        {
                            fprintf(file,"fload ");
                            fprintf(file,"%d\n",lookup_symbol($1)->Address);
                        }
                        else if(strcmp(lookup_symbol($1)->Type,"bool")==0)
                        {
                            fprintf(file,"iload ");
                            fprintf(file,"%d\n",lookup_symbol($1)->Address);
                        }
                        else if(strcmp(lookup_symbol($1)->Type,"string")==0)
                        {
                            fprintf(file,"aload ");
                            fprintf(file,"%d\n",lookup_symbol($1)->Address);
                        }
                        if(ifASSIGN==0)
                        {
                            storeADDRESS=lookup_symbol($1)->Address;
                        }
                    }
                }
    | INT '(' identifier ')' {
                                $$=1;
                                printf("IDENT (name=%s, address=%d)\n",$3,lookup_symbol($3)->Address);
                                printf("F to I\n");
                                fprintf(file,"fload ");
                                fprintf(file,"%d\n",lookup_symbol($3)->Address);
                                fprintf(file,"f2i\n");
                            }
    | FLOAT '(' identifier ')' {
                                $$=2;
                                printf("IDENT (name=%s, address=%d)\n",$3,lookup_symbol($3)->Address);
                                printf("I to F\n");
                                fprintf(file,"iload ");
                                fprintf(file,"%d\n",lookup_symbol($3)->Address);
                                fprintf(file,"i2f\n");
                                }
;
Literal
    : ADD Literal {$$=$2;printf("POS\n");}
    | SUB Literal { 
                        $$=$2;printf("NEG\n");
                        if($$==1)
                        {
                            fprintf(file,"ineg\n");
                        }
                        if($$==2)
                        {
                            fprintf(file,"fneg\n");
                        }
                  }
    | INT_LIT {
                    ifINT=1;

                    if(ifADD_ASIGN==1)
                    {
                        ok=1;
                    }
                   $$=1;
        printf("INT_LIT %d\n",$1);
        LIT_Type=0;
        fprintf(file,"ldc ");
        fprintf(file,"%d\n",$1);
        
        }
    | FLOAT_LIT {
                    $$=2;
                    printf("FLOAT_LIT %f\n",$1);
                    LIT_Type=2;
                    fprintf(file,"ldc ");
                    fprintf(file,"%.6f\n",$1);
                    
                }
    | INT '(' FLOAT_LIT ')' {
                                $$=1;printf("FLOAT_LIT %f\n",$3);
                                printf("F to I\n");
                                fprintf(file,"ldc %.6f\n",$3);
                                fprintf(file,"f2i\n");
                            }
    | FLOAT '(' INT_LIT ')' {
                                $$=2;
                                printf("INT_LIT %d\n",$3);
                                printf("I to F\n");
                                fprintf(file,"ldc %d\n",$3);
                                fprintf(file,"i2f\n");
                            }
    | '"' STRING_LIT '"' {
                            $$=3;printf("STRING_LIT %s\n",$2);
                            LIT_Type=1;
                            fprintf(file,"ldc ");
                            fprintf(file,"\"%s\"\n",$2);
                        }
  /*  | INT '(' Expression ')' {
        fprintf(file,"f2i\n");
     }*/
;

BOOL_LIT
    :
    | NOT BOOL_LIT {
                        printf("NOT\n");
                        fprintf(file,"iconst_1\n");
                        fprintf(file,"ixor\n");
                    }
    | TRUE {
                printf("TRUE\n");
                fprintf(file,"iconst_1\n");
            }
    | FALSE{ 
                printf("FALSE\n");
                fprintf(file,"iconst_0\n");
            }
;


DeclarationStmt
    : VAR identifier Type{ 
                            insert_symbol( $2,$3,yylineno,"-");
                            if(strcmp(lookup_symbol($2)->Type,"int32")==0)
                            {
                                fprintf(file,"ldc 0\n");
                                fprintf(file,"istore ");
                                fprintf(file,"%d\n",lookup_symbol($2)->Address);
                            }
                            else if(strcmp(lookup_symbol($2)->Type,"float32")==0)
                            {
                                fprintf(file,"ldc 0.000000\n");
                                fprintf(file,"fstore ");
                                fprintf(file,"%d\n",lookup_symbol($2)->Address);
                            }
                            else if(strcmp(lookup_symbol($2)->Type,"string")==0)
                            {
                                fprintf(file,"ldc \"\"\n");
                                fprintf(file,"astore ");
                                fprintf(file,"%d\n",lookup_symbol($2)->Address);
                            }
                        }
    | VAR identifier '[' Literal ']' Type{
                                            insert_symbol( $2,"array",yylineno+1,$6);
                                            if(strcmp(lookup_symbol($2)->Element_type,"int32")==0)
                                            {
                                                fprintf(file,"newarray int\n");
                                            }
                                            else if(strcmp(lookup_symbol($2)->Element_type,"float32")==0)
                                            {
                                                fprintf(file,"newarray float\n");
                                            }
                                            fprintf(file,"astore %d\n",lookup_symbol($2)->Address);
                            }
    | VAR identifier Type ASSIGN Expression {
                                                insert_symbol($2,$3,yylineno,"-");
                                                if(strcmp(lookup_symbol($2)->Type,"int32")==0)
                                                {
                                                    fprintf(file,"istore ");
                                                    fprintf(file,"%d\n",lookup_symbol($2)->Address);
                                                }
                                                else if(strcmp(lookup_symbol($2)->Type,"float32")==0)
                                                {
                                                    fprintf(file,"fstore ");
                                                    fprintf(file,"%d\n",lookup_symbol($2)->Address);
                                                }
                                                else if(strcmp(lookup_symbol($2)->Type,"string")==0)
                                                {
                                                    fprintf(file,"astore ");
                                                    fprintf(file,"%d\n",lookup_symbol($2)->Address);
                                                }
                                                
                                            }
    | VAR identifier Type ASSIGN BOOL_LIT {
        insert_symbol($2,$3,yylineno+1,"-");
        if(strcmp(lookup_symbol($2)->Type,"bool")==0)
        {
            fprintf(file,"istore ");
            fprintf(file,"%d\n",lookup_symbol($2)->Address);
        }
    }
;


assign_op
    : ASSIGN{$$=$1;ifASSIGN=1;}
    | ADD_ASSIGN {$$=$1;ifADD_ASIGN=1;if(ifINT==1){beforeINT=1;}}
    | SUB_ASSIGN {$$=$1;}
    | MUL_ASSIGN {$$=$1;}
    | QUO_ASSIGN {$$=$1;}
    | REM_ASSIGN {$$=$1;}
;

IncDecStmt 
    : identifier{
                    ifID=1;
                    if(lookup_symbol($1)==NULL)
                    {
                        HAS_ERROR=1;
                        printf("error:%d: undefined: %s\n",yylineno+1,$1);
                    }
                    else
                    {
                        
                        printf("IDENT (name=%s, address=%d)\n",$1,lookup_symbol($1)->Address); 
                        if(strcmp(lookup_symbol($1)->Type,"int32")==0)
                        {
                            fprintf(file,"iload ");
                            fprintf(file,"%d\n",lookup_symbol($1)->Address);
                        }
                        else if(strcmp(lookup_symbol($1)->Type,"float32")==0)
                        {
                            fprintf(file,"fload ");
                            fprintf(file,"%d\n",lookup_symbol($1)->Address);
                        } 
                    } 
                }
                INC {
                        printf("INC\n");
                        if(strcmp(lookup_symbol($1)->Type,"int32")==0)
                        {
                            fprintf(file,"ldc ");
                            fprintf(file,"1\n");
                            fprintf(file,"iadd\n");
                            fprintf(file,"istore ");
                            fprintf(file,"%d\n",lookup_symbol($1)->Address);
                        }
                        else if(strcmp(lookup_symbol($1)->Type,"float32")==0)
                        {
                            fprintf(file,"ldc ");
                            fprintf(file,"1.000000\n");
                            fprintf(file,"fadd\n");
                            fprintf(file,"fstore ");
                            fprintf(file,"%d\n",lookup_symbol($1)->Address);
                        }
                    }
    | identifier{
                    ifID=1;
                    if(lookup_symbol($1)==NULL)
                    {
                        HAS_ERROR=1;
                        printf("error:%d: undefined: %s\n",yylineno+1,$1);
                    }
                    else
                    {
                        
                        printf("IDENT (name=%s, address=%d)\n",$1,lookup_symbol($1)->Address); 
                        if(strcmp(lookup_symbol($1)->Type,"int32")==0)
                        {
                            fprintf(file,"iload ");
                            fprintf(file,"%d\n",lookup_symbol($1)->Address);
                        }
                        else if(strcmp(lookup_symbol($1)->Type,"float32")==0)
                        {
                            fprintf(file,"fload ");
                            fprintf(file,"%d\n",lookup_symbol($1)->Address);
                        } 
                    } 
                }
      DEC {
                if(strcmp(lookup_symbol($1)->Type,"int32")==0)
                {
                    fprintf(file,"ldc ");
                    fprintf(file,"%d\n",1);
                    fprintf(file,"isub\n");
                    fprintf(file,"istore ");
                    fprintf(file,"%d\n",lookup_symbol($1)->Address);
                }
                else if(strcmp(lookup_symbol($1)->Type,"float32")==0)
                {
                    fprintf(file,"ldc ");
                    fprintf(file,"1.000000\n");
                    fprintf(file,"fsub\n");
                    fprintf(file,"fstore ");
                    fprintf(file,"%d\n",lookup_symbol($1)->Address);
                }
                printf("DEC\n");}
;

PrintStmt
    : PRINT '(' Condition ')' {
                                    printf("PRINTLN bool\n");
                                    fprintf(file,"ifne L_cmp_");
                                    fprintf(file,"%d\n",label_index);
                                    fprintf(file,"ldc \"false\"\n");
                                    fprintf(file,"goto L_cmp_");
                                    fprintf(file,"%d\n",label_index+1);
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index);
                                    fprintf(file,"ldc \"true\"\n");
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index+1);

                                    label_index+=2;
                                    fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                    fprintf(file,"swap\n");
                                    fprintf(file,"invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
                                }
    | PRINTLN '(' Condition ')' {
                                    printf("PRINTLN bool\n");
                                    fprintf(file,"ifne L_cmp_");
                                    fprintf(file,"%d\n",label_index);
                                    fprintf(file,"ldc \"false\"\n");
                                    fprintf(file,"goto L_cmp_");
                                    fprintf(file,"%d\n",label_index+1);
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index);
                                    fprintf(file,"ldc \"true\"\n");
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index+1);

                                    label_index+=2;
                                    fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                    fprintf(file,"swap\n");
                                    fprintf(file,"invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");



                                }
    | PRINTLN '(' Expression ')'{
                                    
                                    fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                    fprintf(file,"swap\n");
                                    if($3==1)
                                    {
                                        fprintf(file,"invokevirtual java/io/PrintStream/println(I)V\n");
                                    }
                                    if($3==2)
                                    {
                                        fprintf(file,"invokevirtual java/io/PrintStream/println(F)V\n");
                                    }
                                }
    | PRINT '(' Expression ')'{

                                    fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                    fprintf(file,"swap\n");
                                    if($3==1)
                                    {
                                        fprintf(file,"invokevirtual java/io/PrintStream/print(I)V\n");
                                    }
                                    if($3==2)
                                    {
                                        fprintf(file,"invokevirtual java/io/PrintStream/print(F)V\n");
                                    }
                                }
                                    
    | PRINTLN '(' IndexExpr ')' {
        
                                    if($3==1)
                                    {
                                        printf("PRINTLN int32\n");
                                    }
                                    else if($3==2)
                                    {
                                        printf("PRINTLN float32\n");
                                    }
                                    else if($3==3)
                                    {
                                        printf("PRINTLN string\n");
                                    }
                                    else if($3==4)
                                    {
                                        printf("PRINTLN bool\n");
                                    }

                                    

                                    
                                    if($3==1)
                                    {
                                        fprintf(file,"iaload\n");
                                        fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                        fprintf(file,"swap\n");
                                        fprintf(file,"invokevirtual java/io/PrintStream/println(I)V\n");
                                    }
                                    if($3==2)
                                    {
                                        fprintf(file,"faload\n");
                                        fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                        fprintf(file,"swap\n");
                                        fprintf(file,"invokevirtual java/io/PrintStream/println(F)V\n");
                                    }
                                    
                                }
    | PRINTLN '(' identifierStmt ')' {
                                        if($3!=0)
                                        {
                                            if($3==1)
                                            {
                                                printf("PRINTLN int32\n");
                                                fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                                fprintf(file,"swap\n");
                                                fprintf(file,"invokevirtual java/io/PrintStream/println(I)V\n");
                                            }
                                            else if($3==2)
                                            {
                                                printf("PRINTLN float32\n");
                                                fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                                fprintf(file,"swap\n");
                                                fprintf(file,"invokevirtual java/io/PrintStream/println(F)V\n");
                                            }
                                            else if($3==3)
                                            {
                                                printf("PRINTLN string\n");
                                                fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                                fprintf(file,"swap\n");
                                                fprintf(file,"invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
                                            }
                                            else if($3==4)
                                            {
                                                printf("PRINTLN bool\n");
                                
                                                fprintf(file,"ifne L_cmp_");
                                                fprintf(file,"%d\n",label_index);
                                                fprintf(file,"ldc \"false\"\n");
                                                fprintf(file,"goto L_cmp_");
                                                fprintf(file,"%d\n",label_index+1);
                                                fprintf(file,"L_cmp_");
                                                fprintf(file,"%d :\n",label_index);
                                                fprintf(file,"ldc \"true\"\n");
                                                fprintf(file,"L_cmp_");
                                                fprintf(file,"%d :\n",label_index+1);

                                                label_index+=2;
                                                fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                                fprintf(file,"swap\n");
                                                fprintf(file,"invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
                                            }
                                        }
                                        
                                    }
    | PRINT '(' identifierStmt ')' {
                                        if($3!=0)
                                        {
                                            if($3==1)
                                            {
                                                printf("PRINT int32\n");
                                                fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                                fprintf(file,"swap\n");
                                                fprintf(file,"invokevirtual java/io/PrintStream/print(I)V\n");
                                            }
                                            else if($3==2)
                                            {
                                                printf("PRINT float32\n");
                                            }
                                            else if($3==3)
                                            {
                                                printf("PRINT string\n");
                                            }
                                            else if($3==4)
                                            {
                                                printf("PRINT bool\n");
                                            }
                                        }
                                    }
    | PRINTLN '(' Literal  ')' {    
                                    if(LIT_Type==0)
                                    {
                                        printf("PRINTLN int32\n");
                                        fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                        fprintf(file,"swap\n");
                                        fprintf(file,"invokevirtual java/io/PrintStream/println(I)V\n");
                                    }
                                    else if(LIT_Type==1)
                                    {
                                        printf("PRINTLN string\n");
                                        fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                        fprintf(file,"swap\n");
                                        fprintf(file,"invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
                                    }
                                    else if(LIT_Type==2)
                                    {
                                        printf("PRINTLN float32\n");
                                        fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                        fprintf(file,"swap\n");
                                        fprintf(file,"invokevirtual java/io/PrintStream/println(F)V\n");
                                    }

                                    
                                }
    | PRINT '(' Literal  ')' {
                                    if(LIT_Type==0)
                                    {
                                        printf("PRINT int32\n");
                                        fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                        fprintf(file,"swap\n");
                                        fprintf(file,"invokevirtual java/io/PrintStream/print(I)V\n");
                                    }
                                    else if(LIT_Type==1)
                                    {
                                        printf("PRINT string\n");
                                        printf("PRINTLN string\n");
                                        fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                        fprintf(file,"swap\n");
                                        fprintf(file,"invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
                                    }
                                    else if(LIT_Type==2)
                                    {
                                        printf("PRINT float32\n");
                                        printf("PRINTLN int32\n");
                                        fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                        fprintf(file,"swap\n");
                                        fprintf(file,"invokevirtual java/io/PrintStream/print(F)V\n");
                                    }
                                    
                             }
    | PRINTLN '(' INT '(' IndexExpr ')' ')' {

                                    if($5==2)
                                    {
                                        fprintf(file,"faload\n");
                                        fprintf(file,"f2i\n");
                                        fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                        fprintf(file,"swap\n");
                                        fprintf(file,"invokevirtual java/io/PrintStream/println(I)V\n");
                                    }
                                    isARRAY=0;
    }
    | PRINTLN '(' INT '(' Expression ')' ')'{ 
                                    fprintf(file,"f2i\n");
                                    fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                                    fprintf(file,"swap\n");
                                    fprintf(file,"invokevirtual java/io/PrintStream/println(I)V\n"); 

                }
;

Condition
    : Expression GTR Expression {
                                    printf("%s\n", $2);
                                    if($1==1)
                                    {
                                        fprintf(file,"isub\n");
                                    }
                                    else if($1==2)
                                    {
                                        fprintf(file,"fcmpl\n");
                                    }
                                    fprintf(file,"ifgt L_cmp_");
                                    fprintf(file,"%d\n",label_index);
                                    fprintf(file,"iconst_0\n");
                                    fprintf(file,"goto L_cmp_");
                                    fprintf(file,"%d\n",label_index+1);
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index);
                                    fprintf(file,"iconst_1\n");
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index+1);
                                    label_index+=2;

                                }
    | Expression LSS Expression {
                                    printf("%s\n", $2);
                                    printf("%s\n", $2);
                                    if($1==1)
                                    {
                                        fprintf(file,"isub\n");
                                    }
                                    else if($1==2)
                                    {
                                        fprintf(file,"fcmpl\n");
                                    }
                                    fprintf(file,"iflt L_cmp_");
                                    fprintf(file,"%d\n",label_index);
                                    fprintf(file,"iconst_0\n");
                                    fprintf(file,"goto L_cmp_");
                                    fprintf(file,"%d\n",label_index+1);
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index);
                                    fprintf(file,"iconst_1\n");
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index+1);
                                    label_index+=2;
                                }
    | Expression LEQ Expression {
                                    printf("%s\n", $2);
                                    if($1==1)
                                    {
                                        fprintf(file,"isub\n");
                                    }
                                    else if($1==2)
                                    {
                                        fprintf(file,"fcmpl\n");
                                    }
                                    fprintf(file,"ifle L_cmp_");
                                    fprintf(file,"%d\n",label_index);
                                    fprintf(file,"iconst_0\n");
                                    fprintf(file,"goto L_cmp_");
                                    fprintf(file,"%d\n",label_index+1);
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index);
                                    fprintf(file,"iconst_1\n");
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index+1);
                                    if(enter_if==1)
                                    {
                                        fprintf(file,"ifeq L_if");
                                        fprintf(file,"%d_false\n",if_index-1);
                                    }
                                    label_index+=2;
                                    //if_index+=1;

                                }
    | Condition LOR Condition {
                                    printf("%s\n", $2);
                                    fprintf(file,"ior\n");
                                }
    | Condition LAND Condition {
                                    printf("%s\n", $2);
                                    fprintf(file,"iand\n");
                                }
    | Literal LAND BOOL_LIT {HAS_ERROR=1;printf("error:%d: invalid operation: (operator LAND not defined on int32)\n", yylineno+1);printf("%s\n", $2);}
    | BOOL_LIT LOR Expression {if($3!=4){HAS_ERROR=1;printf("error:%d: invalid operation: (operator LOR not defined on int32)\n", yylineno);}printf("%s\n", $2);}
    | BOOL_LIT
    | Expression EQL Expression {
                                    printf("%s\n", $2);
                                    if($1==1)
                                    {
                                        fprintf(file,"isub\n");
                                    }
                                    else if($1==2)
                                    {
                                        fprintf(file,"fcmpl\n");
                                    }
                                    fprintf(file,"ifeq L_cmp_");
                                    fprintf(file,"%d\n",label_index);
                                    fprintf(file,"iconst_0\n");
                                    fprintf(file,"goto L_cmp_");
                                    fprintf(file,"%d\n",label_index+1);
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index);
                                    fprintf(file,"iconst_1\n");
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index+1);
                                    if(enter_if==1)
                                    {
                                        fprintf(file,"ifeq L_if");
                                        fprintf(file,"%d_false\n",if_index-1);
                                    }
                                    //if_index+=1;
                                    label_index+=2;
                                }
    | Expression NEQ Expression {
                                    printf("%s\n", $2);
                                    if($1==1)
                                    {
                                        fprintf(file,"isub\n");
                                    }
                                    else if($1==2)
                                    {
                                        fprintf(file,"fcmpl\n");
                                    }
                                    fprintf(file,"ifne L_cmp_");
                                    fprintf(file,"%d\n",label_index);
                                    fprintf(file,"iconst_0\n");
                                    fprintf(file,"goto L_cmp_");
                                    fprintf(file,"%d\n",label_index+1);
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index);
                                    fprintf(file,"iconst_1\n");
                                    fprintf(file,"L_cmp_");
                                    fprintf(file,"%d :\n",label_index+1);
                                    if(enter_if==1)
                                    {
                                        fprintf(file,"ifeq L_if");
                                        fprintf(file,"%d_false\n",if_index-1);
                                    }
                                    //if_index+=1;
                                    label_index+=2;
                                }
    
;

%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    create_symbol();
    yylineno = 0;

    file=fopen("hw3.j","w");
    fprintf(file,".source hw3.j\n");
	fprintf(file,".class public Main\n");
	fprintf(file,".super java/lang/Object\n");
	fprintf(file,".method public static main([Ljava/lang/String;)V\n");
	fprintf(file,".limit stack 100\n");
	fprintf(file,".limit locals 100\n");

    yyparse();

    fprintf(file,"return\n");
 	fprintf(file,".end method\n");

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);

    if (HAS_ERROR)
    {
        remove("hw3.j");
    }
    return 0;
}

static void create_symbol() {
    table *t=(table *)malloc(sizeof(table));
    t->head=NULL;
    t->tail=NULL;
    if(scope==-1)
    {
        t->prev=NULL;
    }
    else
    {
        current->next=t;
        t->prev=current;
    }
    scope+=1;
    current=t;

}

static void insert_symbol(char* Name,char* Type,int Lineno,char* Element_type) {
    int Name_size=strlen(Name);
    int Type_size=strlen(Type);
    int Element_size=strlen(Element_type);
    if(current->head==NULL)
    {   current->head=(symbolTable*)malloc(sizeof(symbolTable));
        strncpy(current->head->Name,Name,Name_size);
        strncpy(current->head->Type,Type,Type_size);
        current->head->Address=address;
        current->head->Lineno=Lineno;
        strncpy(current->head->Element_type,Element_type,Element_size);
        address+=1;
        current->head->next=NULL;
        printf("> Insert {%s} into symbol table (scope level: %d)\n", Name,scope );
    }
    else if(current->tail==NULL)
    {
        if(strcmp(current->head->Name,Name)==0)
        {
            printf("error:%d: %s redeclared in this block. previous declaration at line %d",yylineno,Name,current->head->Address);
            return;
        }
        else
        {
            current->tail=(symbolTable*)malloc(sizeof(symbolTable));
            strncpy(current->tail->Name,Name,Name_size);
            strncpy(current->tail->Type,Type,Type_size);
            current->tail->Address=address;
            current->tail->Lineno=Lineno;
            strncpy(current->tail->Element_type,Element_type,Element_size);
            address+=1;
            current->head->next=current->tail;
            current->tail->next=NULL;
            printf("> Insert {%s} into symbol table (scope level: %d)\n", Name,scope );
        }
    }
    else
    {
        symbolTable *f=current->head;

        for(;f!=NULL;f=f->next)
        {
            
            if(f != NULL && strcmp(f->Name,Name)==0)
            {
                printf("error:%d: %s redeclared in this block. previous declaration at line %d\n",yylineno,Name,lookup_symbol(Name)->Lineno);

                return;
            }
        }
        
        symbolTable *new=(symbolTable *)malloc(sizeof(symbolTable));
        strncpy(new->Name,Name,Name_size);
        strncpy(new->Type,Type,Type_size);
        new->Address=address;
        new->Lineno=Lineno;
        new->next=NULL;
        strncpy(new->Element_type,Element_type,Element_size);
        address+=1;
        current->tail->next=new;
        current->tail=new;
        printf("> Insert {%s} into symbol table (scope level: %d)\n", Name,scope );
    }
}

static symbolTable* lookup_symbol(char* Name) {
    symbolTable* flag = NULL;
    table* find=current;
    
    for(;find!=NULL;find=find->prev)
    {
        symbolTable *temp=find->head;
        while(temp!=NULL && strcmp(temp->Name,Name)!=0)
        {
            temp=temp->next;
        }
        if(temp!=NULL && strcmp(temp->Name,Name)==0)
        {
            flag=temp;
            break;
        }
    }
    return flag;
}

static void dump_symbol() {
    printf("> Dump symbol table (scope level: %d)\n", scope);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n",
           "Index", "Name", "Type", "Address", "Lineno", "Element type");
    int index=0;
    while(current->head!=NULL)
    {

    printf("%-10d%-10s%-10s%-10d%-10d%s\n",
            index, current->head->Name, current->head->Type, current->head->Address, current->head->Lineno, current->head->Element_type);
        current->head=current->head->next;
        index+=1;
    }

    if(scope!=0)
    {
        current=current->prev;
        current->next=NULL;
    }
    scope-=1;
}

