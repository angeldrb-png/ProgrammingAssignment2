makeCacheMatrix <- function(x = matrix()) {
    # 初始化缓存为 NULL，表示尚未计算逆矩阵
    inv <- NULL
    
    # 设置新矩阵的函数
    set <- function(y) {
        x <<- y         # 将新矩阵赋值给上层环境的 x
        inv <<- NULL    # 矩阵已更新，清空旧的缓存
    }
    
    # 获取当前矩阵的函数
    get <- function() x
    
    # 设置逆矩阵到缓存的函数
    setinverse <- function(inverse) inv <<- inverse
    
    # 从缓存获取逆矩阵的函数
    getinverse <- function() inv
    
    # 返回一个包含上述函数的列表
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
    ## 首先尝试从缓存获取逆矩阵
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)  # 缓存存在，直接返回
    }
    
    ## 缓存不存在，开始计算
    data <- x$get()         # 获取原始矩阵
    inv <- solve(data, ...) # 使用 solve() 计算逆矩阵
    x$setinverse(inv)       # 将计算结果存入缓存
    inv                     # 返回逆矩阵
}
