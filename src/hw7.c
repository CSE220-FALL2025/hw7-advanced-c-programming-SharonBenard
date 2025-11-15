#include "hw7.h"
#define MAX 100

//args =  root node and data to be inserted (matrix_sf * mat)
bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if(root == NULL){ //creating new BST 
        //malloc new node at root (use ptr root/NOT local var)
        root = malloc(sizeof(bst_sf));
        root->mat = mat; //node stored @ same ptr --> do not make copy of mat
        root->left_child = NULL;
        root->right_child= NULL;
        
    }
    else if(mat->name <= root->mat->name){ //if arg name< current root name ---> left child
        //recursively call insert_bst_sf for left child
        //check how memory would work for smthing like this
        root->left_child = insert_bst_sf(mat, root->left_child); 
    }
    else { //mat->name >= root->mat->name
        root->right_child = insert_bst_sf(mat, root->right_child);
    }
    //  root->left_child = insert_bst_sf(*mat, root->left_child)
    //else 
    //  root->right_child = insert_bst_sf(*mat, root->left_child)
    return root; //return root after all checks
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if(root == NULL){
        return NULL;
    }
    else if(root->mat->name == name){ //found match
        return root->mat; //returning ptr to matrix
    }
    else if(name < root->mat->name){ //no match check left child
        return find_bst_sf(name, root->left_child);
        //recursively calling until match has been found
    }
    else{ //root->mat->name >= name
        return find_bst_sf(name, root->right_child);
    }

    //return fird_bst_sf ptr for each case
}

void free_bst_sf(bst_sf *root) {
    if(root == NULL){
        return; //backtracking
    }
    //free all nodes of tree(do not delete root untill all nodes have been freed --> Left Right Root)
    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    free(root->mat);
    free(root);
    //free_bst_sf(root); forever recursion
}


//matrix->values points to beginning of 2D array
//use offset to find element at [row][col] via (row* num cols) + col & deref

//add and transpose work bc of local vars but if matrix metadata not initialized them you're returning garbage
//REMEMBER: malloc NEVER initializes memory ---> up to you
matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    //check if they are equal
    if((mat1->num_rows == mat2->num_rows) && (mat1->num_cols == mat2->num_cols)){
        int rows = mat1->num_rows;
        int cols = mat1->num_cols;

        //malloc sum matrix (will cause segmentation error if NOT malloced bc matrix_sf *mat_sum is an uninitialized pointer)
        matrix_sf *mat_sum = malloc(sizeof(matrix_sf)+sizeof(int)*rows*cols);
        mat_sum->name = 0; //prevents garbage val

        //matrix data NOT "inherited" from malloc ---> must explicitly set or else they will contain garbage (resulting matrix invalid so later ops/fxns break)
        mat_sum->num_rows = rows;
        mat_sum->num_cols = cols;
        for(int i=0; i< rows; i++){
            for(int j=0; j< cols; j++){
            int position =  (i*(cols))+j; //can skip to diff rows (since its a 1D array in row major order) mat[i][j]
            *(mat_sum->values + position) = *(mat1->values + position) + *(mat2->values + position);  //Aij =mat1->value[position] == Aij = *(mat1->value + position)
            }
        }
        
        return mat_sum;
    }
    return NULL; //matrices do not have equal dimensions
}


matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    //mat1 = m x k &  mat2 = k x n so mat1*mat2 = mxn (mat1 rows and mat2 cols)
    int rows = mat1->num_rows; //m
    int cols = mat2->num_cols;//n
    int counter = mat1->num_cols; //k (dim both mats share)

    int i=0,j=0,k=0; //initialize counters

    int row_index;
    int col_index;

    matrix_sf *mat_product = malloc(sizeof(matrix_sf)+sizeof(int)*rows*cols);
    //initialize matrix
    mat_product->num_rows = rows;
    mat_product->num_cols = cols;
    mat_product->name = 0; //prevents garbage val

    if(mat1->num_cols == mat2->num_rows){ //if num cols in mat1 = k = num rows in mat2
       
         
        int sum;
        for(i=0; i<rows; i++){
            

            for (j=0; j<cols; j++){
                
                sum = 0; //resets w/ each new col
                
                for(k = 0; k<counter; k++){
                    //indexes updates in K loop
                    row_index = (i*mat1->num_cols)+k; //mat1[i][k]
                    col_index = (k*mat2->num_cols)+j; //mat2[k][j]
                    sum += *(mat1->values + row_index) * *(mat2->values + col_index);
                }
                int prod_index = i * mat_product->num_cols + j;
                *(mat_product->values + prod_index) = sum; //going through matrix normally so for mat_product[i][j] do mat_product->value + ((i*cols)+j); using mat2 cols

            }

        }

        return mat_product;
    }
     
    else{ //when mat1 cols != mat2 rows (or k is !=)
        return NULL;
    }
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    
    int rows = mat->num_rows;
    int cols = mat->num_cols;

    int new_rows = mat->num_cols;
    int new_cols = mat->num_rows;
    //always malloc to prevent segmentation error (using uninitialized ptr)
    matrix_sf *mat_transpose = malloc(sizeof(matrix_sf)+sizeof(int)*new_rows*new_cols);
    mat_transpose->name = 0; //prevent garbage val

    //matrix data NOT "inherited" from malloc ---> must explicitly set or else they will contain garbage (resulting matrix invalid so later ops/fxns break)
    mat_transpose->num_rows = new_rows;
    mat_transpose->num_cols = new_cols;

    for(int i = 0; i< rows; i++){
        for(int j=0; j<cols; j++){
            int reg_position =  (i*cols)+j;
            int t_position = (j*new_cols)+i; //reverse the offset
            *(mat_transpose->values + t_position) = *(mat->values+ reg_position);
        }
    }

    return mat_transpose;
}


//sizeof(expr) = 8 bits
matrix_sf* create_matrix_sf(char name, const char *expr) {
    //parse string expr w/sscanf
    //store info from ssanf into dma struct
    
    int rows, cols;
    int value; //holds int values in matrix for parsing
    int i=0; //counter
    sscanf(expr, "%d %d", &rows, &cols);

    matrix_sf* create_matrix = (matrix_sf*)malloc(sizeof(matrix_sf) + (sizeof(int)*rows*cols));
    create_matrix->name = name;
    create_matrix->num_rows = rows;
    create_matrix->num_cols = cols;
    //create_matrix->values [rows*cols]; //size of flexible array

    while(*expr != '[')
        expr++; //move to starting bracket
    expr++; //starting now at first num;
    
    while(*expr != ']'){ //while we havent reached the end
        while(*expr == ' ' || *expr == '\t' || *expr == ';'){ //skip end brackets, spaces and ;
            expr++;
        }
           
        //checks if the element is a NUMBER
        if(sscanf(expr, "%d", &value) == 1){ //1 means sscanf was succesful(found number to add)
           *(create_matrix->values + i) = value; //added number
           i++;
        } 

        //after storing a number, skip all chars part of it
        //expr must move past the val stored so if it has more than one digit or negative, it wont get stored again
        if(*expr == '-')
            expr++; //skip minus sign (neg number) -->not its own element
        while(*expr >= '0' && *expr <= '9'){
            expr++; //skip digits that were added; ex: 13 will be stored as 13 and not 1 and 3; -5 stored as -5 and not - and 5
        }
    }

    expr++;
 
    
    return create_matrix; //figure out returning this
    //atp know who many elements there are(row*col) so dont need to alloc values --->already in struct alloc block
    //return NULL;
}
 


char stack[MAX]; //global var
int top = -1;

//helper fxns for stack
void push(char current){
    if(top == MAX - 1){
        printf("stack overflow");
        exit(1);
    }
    top++;
    *(stack + top) = current;
}
char pop(){
    char c;
    if(top == -1){
        printf("stack underflow");
        exit(1);
    }
    c = *(stack+top);
    top = top-1;
    return c;
}
int isEmpty(){
    return top == -1;
}
int getPrecedence(char current){
    switch(current){
        case '\'': //transpose
            return 3;
        case '*':
            return 2;
        case '+':
            return 1;
        default:
            return 0; //for open and closed parentheses
    }
}


char* infix2postfix_sf(char *infix) {
    top = -1;//must be -1 before algo (resets stack so its empty in beginning)
    char *postfix = malloc(strlen(infix) + 1); 
    char current, next;
    char *i= infix, *j=postfix; //counter ptrs
    while(*i != '\0'){
        current = *i;
        switch(current){
            case'(':
                push(current);
                break;
            case ')':
                while((next=pop()) != '('){
                    *j = next;
                     j++;
                }
                break;
            case '\'' :
            case '*':
            case '+': //pops ops only when theres an op of lower or == precedence
                while(!isEmpty() && getPrecedence(*(stack+top)) >= getPrecedence(current)){
                    *j = pop(); //pop off top and write into post fix
                    j++;
                }
                push(current); //push the operator (of lower precedence) into stack
                break;
            default:
                if(current != ' ' && current != '\t'){
                *j = current;
                 j++;
                }
        }
        i++;
    }

    //at end of loop --> clears whatever ops left on stack
    while(!isEmpty()){
        *j = pop();
        j++;
    }
    *j = '\0';

    return postfix;
}


//helper fxns for stack of ptrs to matrix sf
matrix_sf *mat_stack[MAX];
int mat_top = -1;

void push_mat(matrix_sf *mat){
    if(mat_top == MAX-1){
        printf("stack overflow for matrix\n");
        exit(1);
    }
    mat_top++;
    *(mat_stack+mat_top) = mat;
}
matrix_sf *pop_mat(){
    if(mat_top == -1){
        printf("stack underflow for matrix\n");
        exit(1);
    }
    
    matrix_sf *mat = *(mat_stack + mat_top);
    mat_top--;
    return mat;
}
int mat_stack_isEmpty(){
    return mat_top == -1;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    
    //infix->postfix
    char *postfix = infix2postfix_sf(expr);

    mat_top =-1;

    char *i = postfix;
    char current;
    while(*i != '\0'){
        current = *i;
        switch(current){
            //when encountering operators, pop matrix off stack, store in temp var that will be used in calcs/repeatedly pushed and popped until the end 
            case '\'':{ //transpose case (highest prec)
                matrix_sf *mat = pop_mat();
                matrix_sf *t = transpose_mat_sf(mat); //send to transpose fxn via temp var

                //free temps NOT BST so check if the struct is an uppercase letter
                if(!isalpha(mat->name)){
                    free(mat);
                }
                push_mat(t); //push the newly calculated matrix 
                break;
            }
            case '+':
            case '*':{
                matrix_sf *mat1 = pop_mat(); //on top of stack (popped first)
                matrix_sf *mat2 = pop_mat(); //next on stack
                //in mult = mat1 pop, mat2 pop, result = mat2*mat1 (MULT NOT COMMUNATIVE)
                matrix_sf * x; //temp ptr for calculating

                if(current == '+'){
                    x = add_mats_sf(mat2,mat1);
                }
                else{
                    x=mult_mats_sf(mat2,mat1);
                }

                //free temps NOT BST so check if the struct is an uppercase letter
                if(!isalpha(mat1->name)){
                    free(mat1);
                }
                if(!isalpha(mat2->name)){
                    free(mat2);
                }
                push_mat(x); //pushes newly calc matrix back into stack
                break;
            }
            default:
                if(isupper(current)){
                    matrix_sf *mat = find_bst_sf(current, root);
                    push_mat(mat); //if its an alphabet, push onto stack
                }
                break;
        }

        i++;
    }
    
    //the resulting matrix is left on stack so pop off & store for return
    matrix_sf *mat_result = pop_mat(); //either r or t (not freed)
    mat_result->name = name; //makes sure that temp name is gone and proper resulting matrix name is stored in bst (useful for finding a node)
    free(postfix); //malloced in infixpostfix so freeing here(last use of the fxn)
    
    return mat_result;
}

matrix_sf *execute_script_sf(char *filename) {
    //read using getline(malloc for each line) & dealloc at end
    //several cases: 1) create a matrix (call create_matrix and insert into bst)
        //2)given expr (involving several matrices) call evaluate_expr and insert into bst

    char *str = NULL;
    FILE *file = fopen("filename", "r"); //read from filename
    size_t max_line_size = MAX_LINE_LEN;
    getline(&str, &max_line_size, file);

    bst_sf *root = NULL;
    matrix_sf *last = NULL; //last created matrix (return ptr to this) ex: C=A*B+D' return ptr to matrixsf struct named C

    while (getline(&str, &max_line_size, file) != -1){ //error or EOP if return -1
        
        //check if line is creating matrix or if its an expr
            //if new matrix: parse, call creatematrixsf, insert into bst
            //if expr: parse name, parse right side, call evaluateexprsf, insert into bst, keep track of the last one (left side goes into last)
        char *i = str;
        while(*i == ' ' || *i == '\t'){ //skips whitespaces and tabs
            i++; //moves i to correct space
        } 

        //locate = sign (left = matrix name, right = either definition or expression )
        char *equals = strchr(i, '='); //strchr finds first occurence of = & returns ptr to its location in the string
        char name = *(i+0); //name is always teh first element in the string
        char *r = equals+1; //r= right hand side (actual definition/exp and comes after =)

        while(isspace(*r)){ //if ptr is on any whitespace, move it
            r++;
        }

        matrix_sf *mat = NULL;
        //to check whether the right side is def or exp--> check if it stars w/ a num or no bc starts w/ num is a row --->definition and if letter-->exp
        if(isdigit(*r)){
            mat=create_matrix_sf(name, r); //pass exp in r
        }
        else{
            mat=evaluate_expr_sf(name, r, root);
        }

        //after we decide either def or exp, insert into bst
        root = insert_bst_sf(mat, root);
        last = mat; //make sure to update last created
    }
    return last;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}


