===========================================
README - ACT-R MODELS
===========================================
Luggage Organization in Car Trunk
===========================================

===========================================
SOURCE FILES
===========================================

1. projet.lisp
-------------------------------------------
Description: Base ACT-R model for luggage organization
              Handles 2D constraints (width and weight only)

Features:
  - Organization of 3 to 6 suitcases on 2 levels
  - Constraints: level 1 width ≤ 12, level 2 weight ≤ level 1 weight
  - Sequential reasoning (category then weight)
  - Learning via base-level learning

Main file to load: projet.lisp
Loading command: (load "projet.lisp")

Main functions:
  - (place-valises n-times &optional draw-valises): Model execution
  - (show-learning n &optional graph block-size): Learning graph
  - (config-actr-params ...): ACT-R parameter configuration

ACT-R model: baggage-organization


2. projet_hauteur.lisp
-------------------------------------------
Description: Advanced ACT-R model for luggage organization
              Handles 3D constraints (width, weight and height)

Features:
  - Organization of 3 to 6 suitcases on 2 levels
  - Constraints: 
    * Level 1 width ≤ 12
    * Level 1 height ≤ 10
    * Total height (level 1 + level 2) ≤ 10
    * Level 2 weight ≤ level 1 weight
  - Parallel reasoning (category AND weight simultaneously)
  - Learning via base-level learning
  - 3D management with height calculation (MAX of suitcases side by side)

Main file to load: projet_hauteur.lisp
Loading command: (load "projet_hauteur.lisp")

Main functions:
  - (place-valises n-times &optional draw-valises): Model execution
  - (show-learning n &optional graph block-size): Learning graph
  - (config-actr-params ...): ACT-R parameter configuration
  - (activer-trace-complete): Activates complete trace for debugging

ACT-R model: baggage-organization-height

Constant:
  - +hauteur-max-coffre+ = 10 (maximum trunk height)


===========================================
ACT-R PARAMETERS (sgp COMMAND)
===========================================

The (sgp ...) command allows configuration of ACT-R model parameters.
These parameters influence behavior, learning and performance.

General syntax:
(sgp :parameter1 value1 :parameter2 value2 ...)

Available parameters:

1. :v (Verbose)
-------------------------------------------
Description: Activates or deactivates verbose trace in console
Possible values:
  - t: Trace enabled (displays all productions and actions)
  - nil: Trace disabled (default, best performance)
  - stream: Redirects trace to specific stream
  - pathname: Redirects trace to file

Example:
  (sgp :v t)        ; Enable trace
  (sgp :v nil)      ; Disable trace (default)


2. :esc (Escape)
-------------------------------------------
Description: Activates or deactivates special character escaping
Possible values:
  - t: Escaping enabled
  - nil: Escaping disabled (default)

Example:
  (sgp :esc t)
  (sgp :esc nil)    ; Default


3. :ans (Activation Noise Scale)
-------------------------------------------
Description: Controls noise level in chunk activation
              Higher value means more model variability
Possible values:
  - Positive number (ex: 0.1, 0.2, 0.5)
  - nil: No noise (deterministic)

Recommended values:
  - 0.05 - 0.1: Low variability (default)
  - 0.2 - 0.5: Moderate variability
  - > 0.5: High variability

Example:
  (sgp :ans 0.1)    ; Moderate noise (default)
  (sgp :ans 0.2)    ; Higher noise


4. :bll (Base-Level Learning)
-------------------------------------------
Description: Controls learning speed of chunks in memory
              Higher value means faster learning
Possible values:
  - Positive number (ex: 0.5, 0.6, 0.7)
  - nil: No learning

Recommended values:
  - 0.5: Moderate learning (default for projet.lisp)
  - 0.6: Faster learning (default for projet_hauteur.lisp)
  - 0.7 - 0.8: Very fast learning

Example:
  (sgp :bll 0.5)    ; Moderate learning
  (sgp :bll 0.7)    ; Fast learning


5. :ncnar (No Chunk Name As Reference)
-------------------------------------------
Description: Controls whether chunk names can be used as references
Possible values:
  - t: Chunk names cannot be referenced
  - nil: Chunk names can be referenced (default)
  - delete: Removes existing references

Example:
  (sgp :ncnar nil)  ; Default
  (sgp :ncnar t)


6. :rt (Retrieval Threshold)
-------------------------------------------
Description: Minimum activation threshold to retrieve chunk from memory
              Higher value means more difficult retrieval
Possible values:
  - Number (ex: 0, 0.1, 0.5, 1.0)
  - 0: No threshold (default, instant retrieval)

Recommended values:
  - 0: Instant retrieval (default, best performance)
  - 0.1 - 0.5: Slower retrieval (to see trace)

Example:
  (sgp :rt 0)       ; Instant retrieval (default)
  (sgp :rt 0.1)    ; Slower retrieval (for debugging)


7. :pas (Production Action Selection)
-------------------------------------------
Description: Controls production selection in case of equality
Possible values:
  - Positive number: Delay before selection
  - nil: No delay (default)

Example:
  (sgp :pas nil)    ; Default


8. :show-focus (Show Focus)
-------------------------------------------
Description: Displays or not the current model focus
Possible values:
  - t: Displays focus (default)
  - nil: Does not display focus

Example:
  (sgp :show-focus t)   ; Default
  (sgp :show-focus nil)


9. :trace-detail (Trace Detail)
-------------------------------------------
Description: Level of detail in execution trace
Possible values:
  - 'high: Very detailed trace (all information)
  - 'medium: Moderately detailed trace
  - 'low: Minimal trace (default)

Example:
  (sgp :trace-detail 'low)     ; Default
  (sgp :trace-detail 'medium) ; Moderate trace
  (sgp :trace-detail 'high)   ; Complete trace (for debugging)


===========================================
RECOMMENDED CONFIGURATIONS
===========================================

Default configuration (projet.lisp)
-------------------------------------------
(sgp :v nil :esc nil :ans 0.1 :bll 0.5 :ncnar nil :rt 0 :pas nil :show-focus t :trace-detail 'low)

Default configuration (projet_hauteur.lisp)
-------------------------------------------
(sgp :v nil :esc nil :ans 0.05 :bll 0.6 :ncnar nil :rt 0 :pas nil :show-focus t :trace-detail 'low)

Configuration for debugging (complete trace)
-------------------------------------------
(sgp :v t :show-focus t :trace-detail 'high :rt 0.1)

Configuration for maximum performance
-------------------------------------------
(sgp :v nil :show-focus nil :trace-detail 'low :rt 0)

Configuration for fast learning
-------------------------------------------
(sgp :bll 0.7 :ans 0.1)


===========================================
config-actr-params FUNCTION
===========================================

Both models include a utility function to configure parameters:

Syntax:
(config-actr-params &key (v nil) (esc nil) (ans 0.1) (bll 0.5) (ncnar nil) (rt 0) (pas nil) (show-focus t) (trace-detail 'low))

Usage examples:
  (config-actr-params)  ; Displays current parameters
  (config-actr-params :v t :trace-detail 'high)  ; Enable complete trace
  (config-actr-params :bll 0.7 :ans 0.2)  ; Fast learning


===========================================
activer-trace-complete FUNCTION
===========================================

Available only in projet_hauteur.lisp

This function automatically configures parameters to see complete trace:
  - Enables :v (trace)
  - Enables :show-focus
  - Sets :trace-detail to 'high

Usage:
  (activer-trace-complete)
  (place-valises 1 t)


===========================================
MODEL LOADING
===========================================

IMPORTANT: Always execute (clear-all) before loading a model!

Recommended order:
1. (clear-all)                    ; Resets ACT-R
2. (load "projet.lisp")          ; OR (load "projet_hauteur.lisp")
3. Configure parameters if necessary
4. Execute the model

Complete example:
  (clear-all)
  (load "projet_hauteur.lisp")
  (config-actr-params :bll 0.7)
  (place-valises 10 t)


===========================================
DIFFERENCES BETWEEN MODELS
===========================================

projet.lisp (2D)
  - Handles only width and weight
  - Sequential reasoning
  - Simpler, more regular learning
  - Default parameters: ans 0.1, bll 0.5

projet_hauteur.lisp (3D)
  - Handles width, weight AND height
  - Parallel reasoning (more human-like)
  - More complex, learning with fluctuations
  - Default parameters: ans 0.05, bll 0.6
  - Additional function: activer-trace-complete


===========================================
FINAL NOTES
===========================================

- Default parameters are optimized for each model
- Modifying parameters can affect performance and learning
- For debugging, use :v t and :trace-detail 'high
- For maximum performance, use default values
- See commands.txt for detailed execution commands
