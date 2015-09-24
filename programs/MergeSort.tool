object MergeSort {
    def main() : Unit = {
        println(new MS().Start(10));
    }
}

class MS {
    var number : Int[];
    var sortNum : Int[];
    var size : Int;
	var aux: Int;
    
    def Start(sz: Int): String = {
    	size = sz;
    	sortNum = new Int[sz];
    	aux = this.Init(sz);
    	sortNum = this.sort(sz,number);
		aux = this.Print();
		
    return "done";
    }
    
    def sort(sz: Int,array: Int[]): Int[] = {
    	var curr:Int;
    	var arr: Int[];
    	var sub1: Int[];
    	var sub2: Int[];
    	var so1: Int[];
    	var so2: Int[];
    	var temp: Int;
    	var i: Int;
    	var re: Int;
    	var j : Int;
 		var k : Int;
    	
    	re  = new Int[sz];
    	
    	arr = array;
           
    	if((sz == 2)||(sz == 1)){
    		if(sz == 2){
    		if(arr[1] < arr[0]){
    			temp = arr[0];
    			arr[0] = arr[1];
    			arr[1] = temp;
    		}
    		re =  arr;
    		}
    		else re = arr;
    	}
    	
    	else{
    		i = 0;
    		k = 0;
    		sub1 = new Int[sz/2];
    		sub2 = new Int[(sz - (sz/2))];
    		so1 = new Int[sz/2];
    		so2 = new Int[(sz - (sz/2))];   		
    		
    		
    		while(i < sub1.length){
    			sub1[i] = arr[i];
    			i = i + 1;
    		}
    		while(i < sz){
    			sub2[k] = arr[i];
    			i = i + 1;
    			k = k + 1;
    		}
    		
    		so1 = this.sort(sz/2,sub1);
    		so2 = this.sort((sz - (sz/2)),sub2);
    		re = this.merge(so1,so2);
    	}
    	return re;
    }
    	
    	
	def merge(s1: Int[], s2: Int[]): Int[] = {
 	    var n: Int;
		var m: Int;
		var k: Int;
		var com: Int[];
		com = new Int[(s1.length+s2.length)];
		
		k = 0;
		n = 0;
		m = 0;
		while(k < com.length){
	    	if(n == s1.length){
	 	   		while(m < s2.length){
					com[k] = s2[m];
					k = k + 1;
					m = m + 1;
				}
			}
			
    		if(m == s2.length){
    			while(n < s1.length){
					com[k] = s1[n];
					k = k + 1;
					n = n + 1;
				}
			}
			
			
			if(s1[n] < s2[m]){
				com[k] = s1[n];
				k = k + 1;
				n = n + 1;
			}else{
				com[k] = s2[m];
				k = k + 1;
				m = m + 1;
			}		
		}
		
		return com;
	}
        
    
    def Print() : Int = {
        var j : Int;

        j = 0 ;
        while (j < (size)) {
            println(number[j]);
            j = j + 1 ;
        }
        
        println("result");
        j = 0 ;
        while (j < (size)) {
            println(sortNum[j]);
            j = j + 1 ;
        }

        return 0 ;
    }
    
    
       // Initialize array of integers
    def Init(sz : Int) : Int = {
        var m:Int;
        size = sz ;
		m = 0;
        number = new Int[sz] ;
        number[0] = 20 ;
        number[1] = 7  ; 
        number[2] = 12 ;
        number[3] = 1 ;
        number[4] = 65 ; 
        number[5] = 11 ;
        number[6] = 7  ; 
        number[7] = 7  ; 
        number[8] = 19 ; 
        number[9] = 87  ;
		

		
        return 0 ;  
    }
    
}
    