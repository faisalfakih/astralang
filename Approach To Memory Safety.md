## Memory Safe Language Design

### Methodology

1. **Pointer Ownership:**
   Every pointer owns the value it points towards. This means that the memory location or object that a pointer points to is intrinsically tied or linked to that pointer.

2. **Handling 'void' or Null Values:**
   When a value becomes `void` or is nullified, its pointer is redirected or moved to a separate designated area in the RAM.

### Process Outline

```plaintext
+---------------------+     +---------------------+     +------------------------+
|                     |     |                     |     |                        |
| Pointer (Owner)     |---->| Value (Owned)       |-----| Separate Memory Region |
|                     |     |                     |     | (For void/null values) |
+---------------------+     +---------------------+     +------------------------+
                               |          |
                               |          | Becomes void/null and
                               |          | is moved to separate memory
                               |          v
                          +------------------------+
                          |                        |
                          | Separate Memory Region |
                          | (For void/null values) |
                          +------------------------+
```

### Details

#### Pointer Ownership

- **Ownership Link:** The pointer and the value it points towards are paired in a way that the value cannot exist without its owner pointer.

- **Memory Safety:** By ensuring ownership and having strict control over how pointers and values are linked, accidental dereferences or unsafe memory access can be minimized.

#### Handling 'Void' or Null Values

- **Special Memory Area:** A specific region of memory is designated to hold pointers that now point to `void` or null. This area is separate from the main memory region where valid objects reside.

- **Redirection:** When an object becomes `void` or is nullified, the pointer is automatically redirected to this special memory area, ensuring that it does not point to invalid or unsafe memory locations in the standard memory space.

- **Garbage Collection (Consideration):** Periodic checks or specific algorithms might be needed to clean up memory and ensure that no memory leaks occur, especially considering values moving to and from the designated void/null memory area.

### Considerations for Further Development

- **Memory Management:** Detailed mechanisms for allocating, deallocating, and managing memory effectively.

- **Performance:** Ensuring the method does not introduce significant performance overhead due to the continuous checks and memory movement.

- **Concurrency:** Designing safe mechanisms for handling pointers and memory in concurrent or multi-threaded contexts to avoid race conditions or deadlocks.

- **Error Handling:** Developing strategies for handling memory access errors, and providing useful debugging information to developers.

