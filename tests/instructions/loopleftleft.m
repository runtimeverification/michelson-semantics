$contract { code { DROP ; 
         PUSH int 2 ;
         LEFT int ;
         LOOP_LEFT {
            PUSH int 2 ;
            ADD ; 
            RIGHT int
         } ; 
         NIL operation ;
         PAIR }; 
storage unit ;
parameter unit ; } ; 
$paramtype unit ;
$storagetype unit ;
$balance 0 ;
$amount 0 ;
$now 0 ;
$myaddr 0 ;
$knownaddrs ;
$sourceaddr 0 ;
$senderaddr 0 ;
$param Unit ;
$storage Unit ;
