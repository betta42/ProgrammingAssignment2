##This function creates a special "matrix" object that can cache its inverse.
##the input is any nxn matrix M

makeCacheMatix<-function(M=matrix(numeric(),nrow = 0,ncol = 0)){      ##initialise empty  matrix M
        In<-NULL         ##initialise inverse matrix In
        ##Define functions on M
        ##set the value of the matrix / to change the assigned value for M within the enviroment
        set<-function(y){
                M<<-y ##assigns the value y to M
                In<<-NULL ##cleans any cached valued of In
        } 
        ## get the value of the Matrix
        get<-function() M 
        ##set the value of the inverse
        setinv<-function(inverse) In<<-inverse
        ##get the value of the inverse
        getinv<- function() In
        ##assign each of the defined functions to an element of a list / 
        ##to be able to call each function using the operator $
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##If the inverse has already been calculated, then the inverse is retrived from the cache.
##The input is again a matrix M stored trought makeCacheMatrix

cacheSolve<- function(M, ...){
        ##get the inverse In stored by makeCacheMatrix
        In<-M$getinv()
        ## if In is not null, we can use it, adding a message that inform us that we are using stored data
        if(!is.null(In)){
                message("getting cached data")
                return(In)
        }
        ##if no iverse was already  stored, we have to compute it
        ## first get the matrix of wich we have to compute the inverse
        data<-M$get()
        ## find the inverse
        In<-solve(data)
        ##use setinv to assign the value of the inverse in the input object
        M$setinv(In)
        In
}