(ns formidable.dom
  (:require [formidable.util :as fu]
            [formidable.parse :as fp]
            [formidable.render :as fr]
            [dommy.core :as d :refer-macros [sel sel1]]
            [dommy.utils :as du]
            [clojure.string :as string]
            [crate.core :as crate])
  (:require-macros [formidable.macros :refer [with-fallback]]))

(defn serialize
  "Returns a form data string for the given form element, suitable for Ajax
  GET/POST, or passing to formidable.parse/parse-params."
  [form-el]
  (->> (for [el (du/->Array (.-elements form-el))
             :let [name (.-name el)]
             :when (not (string/blank? name))]
         (let [node-name (.-nodeName el)
               type (.-type el)
               value (.-value el)]
           (cond
            (and (= "INPUT" node-name)
                 (#{"checkbox" "radio"} type))
            (when (.-checked el)
              (fu/encode-uri-kv name value))

            (and (= "SELECT" node-name)
                 (= "select-multiple" type))
            (->> (for [opt (du/->Array (.-options el))
                       :when (.-selected opt)]
                   (fu/encode-uri-kv name (.-value opt)))
                 (string/join "&"))

            (and (= "INPUT" node-name)
                 (= "file" type))
            nil

            :else
            (fu/encode-uri-kv name value))))
    (remove nil?)
    (string/join "&")))

(defn get-form-el
  "Given a form container element or a form element, returns the form element"
  [container-or-form-el]
  (if (= "FORM" (.-nodeName container-or-form-el))
    container-or-form-el
    (sel1 container-or-form-el "form")))

(defn- build-class-for-errors
  [renderer]
  (str "."
       (string/join "."
                    (string/split (fr/fields-error-class renderer)
                                  #"\s"))))

(defn clear-problems
  "Clears form problems from the DOM"
  [container-or-form-el & [renderer]]
  (let [form-el (get-form-el container-or-form-el)
        error-class (if renderer
                      (build-class-for-errors renderer)
                      ".problem.error")
        classes-to-remove (if renderer
                            (clojure.string/split (fr/fields-error-class renderer)
                                                  #"\s")
                            ["problem" "error"])]
    (when-let [parent-el (.-parentNode form-el)]
      (when-let [problems-el (sel1 parent-el ".form-problems")]
        (d/remove! problems-el)))
    (doseq [el (sel form-el error-class)]
      (apply d/remove-class! el classes-to-remove))))

(defn get-scroll-top
  "Returns the top window scroll position"
  []
  (if (exists? (.-pageYOffset js/window))
    (.-pageYOffset js/window)
    (.-scrollTop (or (-> js/document .-documentElement)
                     (-> js/document .-body .-parentNode)
                     (-> js/document .-body)))))

(defn get-offset-top
  "Returns an element's top offset relative to the window"
  [el]
  (let [rect (.getBoundingClientRect el)]
    (+ (.-top rect) (get-scroll-top))))

(defn scroll-to-el
  "Scrolls the window to an element's offset top"
  [el]
  (.scrollTo js/window 0 (- (get-offset-top el) 10)))

(defn show-problems
  "Shows form problems in the DOM"
  [form-spec container-or-form-el problems]
  (let [form-el (get-form-el container-or-form-el)
        renderer (:renderer form-spec)]
    (clear-problems form-el renderer)
    (let [problems-el (crate/html (fr/render-problems problems
                                                      (:fields form-spec)
                                                      renderer))]
      (d/insert-before! problems-el form-el)
      (scroll-to-el problems-el))
    (doseq [problem problems
            :let [fnames (map name (if (map? problem)
                                     (:keys problem)
                                     [problem]))]
            fname fnames]
      (let [field-container-id (fu/get-field-container-id
                                 {:id (fu/get-field-id {:name fname})
                                  :name fname})]
        (when-let [el (sel1 (str "#" field-container-id))]
          (d/add-class! el
                        (fr/fields-error-class renderer)))))))

(defn handle-submit
  "Attaches an event handler to a form's \"submit\" browser event, validates
  submitted data, then:
    * If validation fails, shows the problems (or, if provided, calls a custom
      failure function with the problems data as the argument)
    * If validation succeeds, calls a success function with parsed params as
      the argument"
  [form-spec container-or-form-el success & [failure]]
  (let [form-el (get-form-el container-or-form-el)
        failure (or failure
                    #(show-problems form-spec form-el %))]
    (d/listen! form-el :submit
               (fn [event]
                 (.preventDefault event)
                 (with-fallback failure
                   (clear-problems form-el (:renderer form-spec))
                   (success
                    (fp/parse-params form-spec (serialize form-el))))))))
