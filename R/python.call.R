#########################################################
# CGB, 20100718, created
#########################################################

python.call <- function( py.foo, ... ){

    foo.args <- list( ... )

    if( is.null( names( foo.args ) ) )
        which.dict <- rep( FALSE, length( foo.args ) )
    else
        which.dict <- names( foo.args ) != ""

    n.args.vect <- sum( !which.dict )
    n.args.dict <- sum(  which.dict )

    #foo.args.dict <- toJSON( foo.args[  which.dict ] )
    #foo.args.vect <- toJSON( foo.args[ !which.dict ] )

    foo.args.dict <- toJSON( foo.args[  which.dict ], collapse = " " )
    foo.args.vect <- toJSON( foo.args[ !which.dict ], collapse = " " )

    # Creating the call

    python.command <- c( 
        paste( "_r_args_dict ='", foo.args.dict, "'", sep = "" ),
        paste( "_r_args_vect ='", foo.args.vect, "'", sep = "" ),
        "_r_args_dict = json.loads( _r_args_dict )",
        "_r_args_vect = json.loads( _r_args_vect )",
        python.command <- paste( "_r_call_return = ", py.foo, "(",
                                  ifelse( n.args.vect == 1, "_r_args_vect[0]", "*_r_args_vect" ),
                                  ifelse( n.args.dict == 0, ")", ", **_r_args_dict)" ), 
                                  sep = "" )
    )

    python.command <- paste( python.command, collapse = "\n" )

    python.exec( python.command )                       # Checks for errors

    ret <- python.get( "_r_call_return" )

    if( length( ret ) == 1 ) ret <- ret[[1]]

    ret
}

