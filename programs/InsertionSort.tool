object InsertionSort {
    def main() : Unit = {
        println(new IS().Start(10));
    }
}

// This class contains the array of integers and
// methods to initialize, print and sort the array
// using InsertionSort
class IS {
    var number : Int[];
    var sortNum : Int[];
    var size : Int;

    // Invoke the Initialization, Sort and Printing
    // Methods
    
    def Start(sz : Int) : Int = {
        var aux01 : Int;
        aux01 = this.Init(sz);
        
        aux01 = size - 1 ;
        aux01 = this.Sort();

        return 9999;
    }


    // Sort array of integers using InsertionSort method
    def Sort() : Int = {
        var i : Int;
        var j : Int;

        var aux03 : Int;

		i = 0;
		sortNum[i] = number[i];
		
		i = 1;
		while(i < (size)){
			
			j = this.findPlace(i);

			
			if(j < i) aux03 = this.insertNum(j,i);
			// number[i] < sortNum[j], place at j
			
			else aux03 = this.appendNum(i,j);
			i = i + 1;
		}
		aux03 = this.Print();
		return 0;
	}
	
	def insertNum(k : Int, i: Int): Int = {
		var v : Int;
		v = i;

		while(! (k == v)){
			sortNum[v] = sortNum[v - 1];
			
			v = v - 1;
		}
		// now i = k
		
		sortNum[k] = number[i];
		return 0;
	
	
	}
	
	
	def findPlace(i: Int): Int = {
		var in : Int;
        var t : Int;
		in = number[i];
		t = 0;
		while(t < i  && sortNum[t] < in && (!(sortNum[t] == in))){
			t = t + 1;
		}
		return t;
	}
	
	
	def appendNum(ii: Int, p: Int):Int = {
		sortNum[p] = number[ii];
		return 0;
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
		sortNum = new Int[sz];
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
		
		while(m < size){
		sortNum[m] = 0;
		m = m + 1;
		}
		
        return 0 ;  
    }
    
}
