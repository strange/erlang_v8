-module(test_suite).                                                            
                                                                                
-include_lib("eunit/include/eunit.hrl").                                        
                                                                                
all_test() ->                                                                   
    application:start(sasl),                                                    
    application:start(erlang_v8),                                               
    [{module, port_tests}].  
