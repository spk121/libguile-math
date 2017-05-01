

;; this->max_size() -
;; this->capacity()
;; this->_M_impl._M_start
;; this->_M_impl._M_finish
;; this->_M_get_Tp_allocator()
;; this->_M_impl._M_end_of_storage
;;

;; reserve (n) -- expand storage to n counts

;; emplace_back

;; VECTOR
;; (constructor)
;; (destructor)
;; operator= on other vector -- copy assignment
;; operator= on other vector -- move assignment?
;; operator= on list -- assignment
;; (assign count value) -- replaces contents with count copies of value
;; (assign first last) -- iterator
;; (assign list) -- replaces contents with list
;; (get_allocator) -- return the allocator for this

;; (at ) -- return the element, with bounds checking
;; operator[] pos -- return the element
;; (front ) -- return the 1sst element
;; (back ) -- return the last element
;; (data ) -- return the underlying storage vector directly

;; iterators, begin to end

;; empty?
;; size - return number of elements
;; max_size - returns max num of elements
;; capacity - returns currently allocated vector's length
;; reserve - set capacity
;; shrink-to-fit - reduce capacity to be size

;; clear -- clears the contents
;; insert -- inserts elements
;; emplace -- constructs elements in place
;; erase -- erases elements
;; push_back -- adds an element to the end
;; emplace_back -- constructs an element in place at the end
;; pop-back -- removes the last element
;; resize -- changes the number of elements
;; swap -- swaps the contents

;; lexicographic operators
;; ==, != <, <=, >, >=

;; swap -- swaps the contents of two vectors
