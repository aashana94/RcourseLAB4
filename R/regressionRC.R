Account <- setRefClass("Account",
                       fields = list(balance = "numeric"),
                       methods= list(
                                withdraw = function(x) {
                                        balance <<- balance - x
                                },
                                deposit = function(x) {
                                  balance <<- balance + x
                                }
                      )
)
