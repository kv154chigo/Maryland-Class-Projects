use std::cmp::Ordering;
use std::collections::HashMap;
use std::mem::swap;

pub trait PriorityQueue<T: PartialOrd> {
    fn enqueue(&mut self, ele: T) -> ();
    fn dequeue(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
    fn makeHeap(&mut self, ele: usize) -> ();
}

/**
    An optional definition of a Node struct you may find useful
**/
struct Node<T> {
    priority: i32,
    data: T,
}

/**
    These traits are implemented for Nodes to make them comparable
**/
impl<T> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Node<T>) -> Option<Ordering> {
        self.priority.partial_cmp(&other.priority)
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Node<T>) -> bool {
        self.priority == other.priority
    }
}

/**
    You must implement the above trait for the vector type
**/
impl<T: PartialOrd> PriorityQueue<T> for Vec<T> {
    /**
        This functions pushes a given element onto the queue and
        reorders the queue such that the min heap property holds.
        See the project specifications for more details on how this
        works.
    **/
    fn enqueue(&mut self, ele: T) -> () {
       self.push(ele);
       let mut count: usize = self.len() -1;

       while count != 0 && self[(count-1)/2] > self[count] {
        self.swap(count, (count-1)/2);
        count = (count -1)/2;
       }
    }  

    /**
        This function removes the root element from the queue and
        reorders the queue such that it maintains the min heap
        property.  See the project specifications for more details.
        You should return the deleted element in the form of an option.
        Return None if the queue was initially empty, Some(T) otherwise.
    **/
    fn dequeue(&mut self) -> Option<T> {
       if self.len() == 0 {
        return None
       }else{
        let mut count: usize = self.len() -1;
        self.swap(0, count);
        let returner = self.remove(self.len() -1);        

        self.makeHeap(0);

        return Some(returner);
    }
    }

     fn makeHeap(&mut self, ele: usize) -> () {
        let mut start: usize = ele; 
        if ((2 * ele) + 1) < self.len() && self[(2 * ele) + 1] < self[ele] {
            start = ((2 * ele) + 1); 
        }
        if ((2 * ele) + 2) < self.len() && self[(2 * ele) + 2] < self[start] {
            start = ((2 *ele) + 2); 
        }
        if (start != ele) 
        { 
        self.swap(ele, start); 
            self.makeHeap(start); 
        }   
    }  


    /**
        This function returns the element that would be removed
        if dequeue were called on the queue.  There should be no
        mutations to the queue.  Return the element in the form
        of an option.  Return None if the queue is empty, Some(T)
        otherwise.
    **/
    fn peek(&self) -> Option<&T> {
        if self.len() == 0{
            return None
        }else{
        return Some(&self[0]);
        }
    }
}


/**
    You must implement this function that computes the orthogonal
    distance between two coordinates.  Remember, orthogonal distance
    is not like Euclidean distance.  See the specifications for more
    details.
**/
pub fn distance(p1: (i32,i32), p2: (i32,i32)) -> i32 {
    match p1{
        (x,y) => match p2 {
            (a,b) => return ((x-a).pow(2) as f64).sqrt() as i32 + ((y-b).pow(2) as f64).sqrt() as i32
        }
    }
}

/**
    You must implement this function that determines which enemy Stark
    should battle and their coordinates.  You are given two hashmaps for
    allies and enemies.  Each maps a name to their current coordinates.
    You can assume that the allies hashmap will always have a name
    called "Stark" included.  Return the name and coordinates of the enemy
    Stark will battle in the form of a 3-tuple.  See the specifications
    for more details on how to choose which enemy.
**/
pub fn target_locator<'a>(allies: &'a HashMap<&String, (i32,i32)>, enemies: &'a HashMap<&String, (i32,i32)>) -> (&'a str,i32,i32) {
    let mut all = Vec::new();
    // let mut allies2 = Vec::new();
    // let mut cord: (i32,i32) = (0,0);
    let mut enemies2 = Vec::new();
    let mut allies2 = Vec::new();


    for j in allies.iter(){

        match j {

            (x,(y,z)) => {
            for i in enemies.iter(){
                 match i {
                     (a,(b,c)) => {
                         all.enqueue(Node {priority: distance((*y,*z),(*b,*c)) , data: x.as_str()});
                     }
                 }
            }
         }
        }
    }

    while all.len() > 0 {

            
        for j in allies.iter(){

                match j {

                    (x,(y,z)) => {
                    for i in enemies.iter(){
                         match i {
                             (a,(b,c)) => if distance((*y,*z),(*b,*c)) == all[0].priority{
                                  if x.as_str() == "Stark" && enemies2.contains(a) == false{
                                    return (a.as_str(),*b,*c);
                                  }else{
                                    if enemies2.contains(a) == false && allies2.contains(x) == false{
                                        enemies2.push(a);
                                        allies2.push(x);
                                      }
                                  }
                                }
                             }
                         }
                    },
                }
                }
                all.dequeue();
            }
          return("",0,0);  
}
    // for i in allies.iter(){
    //     match i {
    //         (a,(b,c)) => if a.as_str() == "Stark"{
    //             let cord = (b,c);
    //         }
    //     }
    // }
    // for i in enemies.iter(){
    //     match i {
    //         (a,(b,c)) => {
    //             all.enqueue(Node {priority: distance(cord,(*b,*c)) , data: a.as_str()});
    //         }
    //     }
    // }
            
    // while all.len() > 0 {
    //     let mut help: bool = true;

    //     for i in allies.iter() {
            
    //         match i {
    //             (a,(b,c)) => if a.as_str() != "Stark"{
                    
    //                 for ele in enemies.iter(){
    //                     match ele{
    //                         (x,(y,z)) => if distance(cord,(*y,*z)) == all[0].priority {
    //                             if distance((*y,*z),(*b,*c)) < all[0].priority && allies2.contains(a) == false{
    //                                 help = false;
    //                                 allies2.push(a);
    //                             }
    //                         }
    //                     }
    //                 }

    //             }
    //         }

    //     }
    //     if help {
    //          for ele in enemies.iter(){
    //                     match ele{
    //                         (x,(y,z)) => if distance(cord,(*y,*z)) == all[0].priority {
    //                             return (x.as_str(),*y,*z);
    //                         }
    //                     }
    //                 }
    //     }
    //     help = true;
    //     all.dequeue();
    // }
    // return("",0,0);

//}
