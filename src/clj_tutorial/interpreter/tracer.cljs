

(ns ^:figwheel-hooks clj-tutorial.interpreter.tracer
  (:require
   [clj-tutorial.utils :as u]
   
   [clojure.repl :refer [source]]
   [clojure.pprint :refer [pprint]] ))

;;#############################################################
;; Tracer
;;#############################################################

(def ^:dynamic *evaluation-states*)


(defn add-evaluation-state!
  [state]
  (swap! *evaluation-states* conj state))


(defn record-pre-evaluation-form!

  "Records a snapshot of the 
   state pre-evaluation state to."
  
  [form env]
  
  (let [state {:state-id (gensym "state_")
               :token-id (:token-id (meta form))
               :form form
               :env env
               ;;:globals @*global-vars*
               :type :a}]
    
    (add-evaluation-state! state)
    state))


(defn record-post-evaluation-form! 

  "Records a snapshot of the 
   state post-evaluation."
  
  [state form*]

  (let [form (:form state)
        rt (cond
             (symbol? form) :resolved
             :else :return)]

    (-> state
        (assoc :return-type rt)
        (assoc :result form*)
        (add-evaluation-state!))))

(defn record-bindings!

  "Logs a binding of a
   symbol to a value."
  
  [binding-map type]
  
  (let [state {:type :binding
               :binding-type type
               :bindings binding-map}]
    
    (add-evaluation-state! state)))

;;------------------------------------
;; Standard Macros
;;------------------------------------
;; Macros currently work like f-expressions,
;; and expand by the f-expand multimethod. 
;;
;; Currently, they are: cond, when, and, or

(defn cond*
  [& clauses]
  (when clauses
    (list 'if (first clauses)
          (if (next clauses)
            (second clauses)
            (throw "cond requires an even number of forms"))
          (cons 'cond (next (next clauses))))))

(defn when*
  "Evaluates test. If logical true, evaluates body in an implicit do."
  {:added "1.0"}
  [test & body]
  (list 'if test (cons 'do body)))


(defn and*
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical false (nil or false), and returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (and) returns true."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & nxt]
 
   (list 'let
         ['x* x]
         (list 'if
               'x*
               (list 'apply 'and nxt)
               'x*))))

(defn or*
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical true  returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (or*) returns true."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & nxt]
 
   (list 'let
         ['x* x]
         (list 'if
               'x*
               'x*
               (list 'apply 'or nxt)))))

(def macro?
  {'cond #'cond*
   'when #'when*
   'and #'and*
   'or #'or*})

(defn f-expand
  [[f & args :as form]]
  (if-let [f (macro? f)]
    (apply f args)
    form))


;;#############################################################
;; Core Evaluator
;;#############################################################

(declare evaluate -evaluate)

(def ^:dynamic *global-vars*)

(def built-ins
  {'+ #'+
   '- #'-
   '* #'*
   '/ #'/
   '= #'=
   
   'first #'first
   'rest #'rest
   'conj #'conj
   'list? #'list?})

(def default-locals
  {:locals {}})

(defn gen-evaluation-stream
  
  "Previously a continuation passing style
   state machine.

   Now, it just runs through the whole evaluation,
   storing execution order in an atom bound to 
   *evaluation-states*.

   Also initiates a global environment to hold
   reference (Var and Atom) bindings."

  ([form]
   (gen-evaluation-stream form default-locals))

  ([form env]
   
   (binding [*evaluation-states* (atom [])
             *global-vars* (atom built-ins)]
     
     (evaluate form env)
     
     @*evaluation-states*)))

(defn evaluate  
  
  "1) Records current evaluation 
   state pre-evaluation.
  
   2) Calls the -evaluate 
   helper (which will in turn call
   evaluate), which returns the 
   evaluated form.
  
   3) Records the post-evaluation 
   value.
  
   4) Returns evaluated form."
  
  [form env]
  
  (let [state (record-pre-evaluation-form! form env)
        form* (-evaluate form env)]

    (record-post-evaluation-form! state form*)
    
    form*))

;;------------------------------------
;; Environment & Bindings
;;------------------------------------

(defn destructure
  [params args]
  {params args})



(defn add-binding-to-env

  "Binds values to symbols,
   destructuring if needed.

   Logs binding in evaluation
   stream.
  
   Returns updated environment."
  
  [env params val]
  
  (let [binding-map (destructure params val)]

    (record-bindings! binding-map :locals)
    
    (update env :locals merge binding-map)))

(defn add-binding-to-global-env!

  "Binds values to global symbols,
   destructuring if needed.

   Logs binding in evaluation
   stream.
  
   Returns updated environment."
  
  [sym val]

  (record-bindings! {sym val} :globals)
  
  (swap! *global-vars* assoc sym val))

(defn get-var
  [sym]
  (when *global-vars*
    (get @*global-vars* sym)))

(defn resolve-sym
  [sym env]
  (or (get-in env [:locals sym])
      (get-var sym)     
      (throw
       (str "Error :: Symbol " sym " not found."))))

;;------------------------------------
;; Tail-Call Tagging
;;------------------------------------
;;
;; Indicates in environment that the current
;; computation is in the tail position.

(defn in-tail-position? 
  [env]
  (::tail-call? env))

(defn mark-as-tail-call
  [env]
  (assoc env ::tail-call? true))

(defn unmark-as-tail-call
  [env]
  (assoc env ::tail-call? false))

(defn count-params

  "Counts all non-& elements
   in param list."
  
  [params]
  
  (letfn [(valid-sym? [s]
            (or (map? s)
                (vector? s)
                (and (symbol? s)
                     (not= '&))))]
    
    (count (filter valid-sym? params))))
      
(defn tag-recur-point 
  [env syms body]
  
  (let [sym-count (count-params syms)
        recur-point {:syms syms
                     :sym-count sym-count
                     :body body}]
    (-> env
      (assoc :recur-point recur-point)
      (mark-as-tail-call))))

;;------------------------------------
;; Lambdas
;;------------------------------------

(defrecord Lambda [fname params body lexical-env])

(defn lambda?
  [form]
  (instance? Lambda form))

(defn invoke-lambda
  [f args env]
  (let [{:keys [params lexical-env body]} f
        bindings (zipmap params args)
 
        env* (-> env
                 (update :locals merge (:locals lexical-env))
                 (update :locals merge bindings))]
     
      (evaluate body env*)))

      
;;------------------------------------
;; Constants & Collections
;;------------------------------------

(defn evaluate-in-env

  "Evaluates collection of
   forms sequentially.

   This is never called for forms in
   tail position, and the environment
   is marked as such."
  
  [forms env]
  
  (let [env* (unmark-as-tail-call env)]
    
    (loop [[f & fs] forms
           acc []]
      
      (if f
        
        (recur fs (conj acc (evaluate f env*)))
        
        acc))))

(defn evaluate-collection
  
  "Evaluations every element
   in the collection while preserving
   meta-data."
  
  [forms env reassembly-fn]
  
  (-> forms
    (evaluate-in-env env)
    (reassembly-fn)
    (with-meta (meta forms))))


(defmulti -evaluate
  u/form->type)

(defmethod -evaluate :default
  [form _]
  form)

(defmethod -evaluate :constant
  [form _]
  (:val form))

(defmethod -evaluate :symbol
  [sym env]
  (resolve-sym sym env))

(defmethod -evaluate :vector
  [form env]
  (evaluate-collection form env vec))

(defmethod -evaluate :set
  [form env]
  (evaluate-collection form env set))

(defmethod -evaluate :map
  [form env]
  (let [reassembly-fn (partial into {})]
    (evaluate-collection form env reassembly-fn)))

(declare -evaluate-sexpr)

(defmethod -evaluate :list
  [form env] 
  (let [form* (f-expand form)]   
    (-evaluate-sexpr form* env)))
      
;;------------------------------------
;; S-Expressions & Special Forms
;;------------------------------------
;; Priorities 
;; 1) if, do, let, def, fn, quote
;; 2) loop, recur, letfn, var
;; 
;; 3) throw, try, catch
;; 4) multiple / variadic arities
;; 5) destructuring
;; 6) continuations


(defmulti -evaluate-sexpr 
  first)

(defmethod -evaluate-sexpr :default
  [[f & args] env]
  
  (let [f* (evaluate f env)
        args* (evaluate-in-env args env)]

    (if (lambda? f*)
      (invoke-lambda f* args* env)
      (apply f* args*))))

(defmethod -evaluate-sexpr 'quote
  [[_ form] env]
  form)

(defmethod -evaluate-sexpr 'var
  [[_ form] env]
  (get-var form))

(defmethod -evaluate-sexpr 'if
  [[_ pred then else] env]

  (let [p-env (unmark-as-tail-call env)
        branch (if (evaluate pred p-env)
                 then 
                 else)]
    
    (evaluate branch env)))

(defmethod -evaluate-sexpr 'do
  [[_ & forms] env]

  (let [statements (butlast forms)
        return (last forms)]

    (evaluate-in-env statements env)   
    
    (evaluate return env)))

(defn add-sequential-binds
  [bindings env]

  (let [tc? (::tail-call? env)]
    
    (loop [[b & bs] (partition 2 bindings)
           env (unmark-as-tail-call env)]
      
      (if-not b
        
        (assoc env ::tail-call? tc?)
        
        (let [[sym val] b
              val* (evaluate val env)
              env* (add-binding-to-env env sym val*)]
          
          (recur bs env*))))))
      
(defmethod -evaluate-sexpr 'let
  [[_ bindings body] env]
  
  (let [env* (add-sequential-binds bindings env)]
    
    (evaluate body env*)))

(defmethod -evaluate-sexpr 'loop
  [[_ bindings body] env]
  
  (let [syms (->> bindings
                  (partition 2)
                  (map first))
        _ (pprint syms)
        env* (tag-recur-point env syms body)
        env* (add-sequential-binds bindings env*)] 
    
    (trampoline (fn thunk []
                  (evaluate body env*)))))

(defn check-if-in-tail-pos 
  [env]
  
  (when-not (in-tail-position? env)
    
    (throw
     
     (str  "Recur can only be called only from the tail position."))))

(defn check-recur-arg-count
  [sym-count args]
  
  (let [arg-count (count args)]
    
    (when-not (= sym-count arg-count)
      
      (throw 
       
       (str "Recur expects " sym-count " args, but received " arg-count ".")))))

(defmethod -evaluate-sexpr 'recur
  [[_ & args :as form] env]
  
  (let [rp (:recur-point env)
        {:keys [syms sym-count body]} rp]
    
    (check-if-in-tail-pos env)
   ;; (check-recur-arg-count sym-count args)

    (let [bindings (zipmap syms args)
          env* (add-sequential-binds bindings env)]
      
      (fn thunk [] 
        (evaluate body env*)))))

(defmethod -evaluate-sexpr 'letfn

  [[_ bindings body] env]

  (let [split-fnames (juxt first
                           (fn [_ & rst]
                             (cons 'fn rst)))
        
        [fnames methods] (map split-fnames bindings)

        bindings* (interleave bindings methods)
        
        env* (add-binding-to-env env fnames ::self)
        env* (add-sequential-binds bindings* env)]

    (evaluate body env*)))



(defmethod -evaluate-sexpr 'fn
  [[_ & [fst & rst :as form]] env]
  
  (let [[fname methods] (if (symbol? fst)
                          [fst rst]
                          [nil form])
        
        env (if fname
              (add-binding-to-env env fname ::self)
              env)

        [params body] methods
        pcount (count params)
        env (tag-recur-point env params body)]

    (->Lambda fname params body env)))

(defmethod -evaluate-sexpr 'def
  [[_ sym val] env]
  
  (let [val* (evaluate val env)]
  
    (add-binding-to-global-env! sym val*)
    
    val*))
