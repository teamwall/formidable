(ns formidable.render.bootstrap3
  (:require [formidable.data :as data]
            [formidable.render :refer [render-form render-field
                                       render-problems fields-error-class
                                       render-compound render-select
                                       build-opt-tag get-input-attrs
                                       get-field-label render-input-val]]
            [formidable.util :as fu]
            [clojure.string :as string]))

(defmethod render-compound :bootstrap3-stacked [field _]
 (let [subfields (for [subfield (:fields field)]
                    (let [sfname (str (:name field) "[" (:name subfield) "]")]
                      (assoc subfield
                             :name sfname
                             :id (fu/get-field-id {:name sfname}))))
        combiner (or (:combiner field)
                     (fn [subfields]
                       [:span.compound
                        (interpose (:separator field " ") subfields)]))]
    (combiner (map (fn [subfield]
                       (let [sfname (str (:name field) "[" (:name subfield) "]")
                             id     (fu/get-field-id {:name sfname})]
                         (vector :div.form-group
                                 [:label.sr-only {:for id} (:label subfield)]
                                 (render-field subfield :bootstrap3-stacked))))
                    subfields))))

(defmethod render-select :bootstrap3-stacked [field _]
  (let [attrs (get-input-attrs field [:name :id :class :autofocus
                                      :disabled :multiple :size :readonly
                                      :tabindex :onchange :onclick :onfocus
                                      :onblur])
        attrs (assoc attrs :class (str (:class attrs) " form-control"))
        val (render-input-val field)
        opts (fu/normalize-options (:options field))
        opts (if (:first-option field)
               (concat (fu/normalize-options [(:first-option field)])
                       opts)
               opts)
        opt-tags (for [[v text subopts] opts]
                   (if (empty? subopts)
                     (build-opt-tag v text val)
                     [:optgroup {:label text}
                      (for [[v text] (fu/normalize-options subopts)]
                        (build-opt-tag v text val))]))
        placeholder (if (true? (:placeholder field))
                      "Select one..."
                      (:placeholder field))
        opt-tags (if (and placeholder (string/blank? val))
                   (cons [:option {:value "" :disabled true :selected true}
                          placeholder]
                         opt-tags)
                   opt-tags)]
    [:select attrs opt-tags]))

(defn render-bootstrap-row [field]
  (let [field-id (fu/get-field-id field)
        checkbox? (= :checkbox (:type field))
        inline? (or (= :compound (:type field))
                    (= :date-select (:type field))
                    (= :time-select (:type field)))
        field (assoc field
                     :id field-id
                     :class (if checkbox? "" "form-control"))
        field (if (= :submit (:type field))
                (assoc field :class (str (:class field)
                                         " btn btn-primary"))
                field)
        content (if (= :heading (:type field))
                  (when (:text field) [:legend (render-field field :bootstrap3-stacked)])
                  (list
                    (when (and (not checkbox?) (:label field))
                      (if inline?
                        [:label.control-label.show {:for field-id} (:label field)]
                        [:label.control-label {:for field-id} (:label field)]))
                    (when (:prefix field)
                       [:span.prefix (:prefix field)])
                    (if checkbox?
                      [:label {:for field-id} " "
                        (render-field field :bootstrap3-stacked) " "
                        [:span.cb-label (:label field)]]
                      (render-field field :bootstrap3-stacked))
                    (when (:suffix field)
                       [:span.suffix (:suffix field)])
                    (when (and (= :submit (:type field))
                                (:cancel-href field))
                       [:span.cancel-link " " [:a.btn {:href (:cancel-href field)}
                                               (:cancel-label field)]])
                    (when (:note field)
                       [:div.note.help-inline (:note field)])))]

    [:div {:id (fu/get-field-container-id field)
           :class (str (cond
                         checkbox? "checkbox "
                         :else     "form-group ")
                       (:div-class field)
                       (when (:problem field) " has-error problem " ))}
      (if inline?
        [:div.form-inline content]
        content)]))

(defn- group-fieldsets [fields]
  (loop [ret []
         group []
         fields fields]
    (if (empty? fields)
      (if (seq group)
        (conj ret group)
        ret)
      (if (#{:heading :submit} (:type (first fields)))
        (recur (if (seq group) (conj ret group) ret)
               [(first fields)]
               (rest fields))
        (recur ret
               (conj group (first fields))
               (rest fields))))))

(defmethod render-problems :bootstrap3-stacked
  [problems & [fields]]
  (let [problems (if (map? problems) [problems] problems)
        fields-by-name (if (map? fields)
                         fields
                         (into {} (for [f fields]
                                    [(:name f) f])))]
    [:div.form-problems.alert.alert-danger.alert-block.clearfix.has-error
     [:ul
      (for [{:keys [keys msg]} problems
            :when msg]
        (let [field-labels (map #(get-field-label
                                   (or (fields-by-name %)
                                       (fields-by-name (name %))
                                       {:name %}))
                                keys)]
          [:li.control-label
           (when (seq field-labels)
             (list [:strong (string/join ", " field-labels)] ": "))
           msg]))]]))

(defn render-bootstrap-form [form-attrs fields class opts]
  (let [[hidden-fields visible-fields] ((juxt filter remove)
                                        #(= :hidden (:type %)) fields)
        submit-only? (and (= 1 (count visible-fields))
                          (= :submit (:type (first visible-fields))))
        extra-attrs {:class (str class
                                 (when submit-only? " submit-only"))}
        form-attrs (merge-with #(str %1 " " %2) form-attrs extra-attrs)]

    [:form (dissoc form-attrs :renderer)
      (when-let [problems (:problems opts)]
       (when (map? (first problems))
         (render-problems problems fields :bootstrap3-stacked)))
      (list
       (map (fn [field] (render-field field :bootstrap3-stacked))
            hidden-fields)
       (for [fieldset (group-fieldsets visible-fields)]
         [:fieldset {:class (str "fieldset-" (name (:name (first fieldset))))}
          (map render-bootstrap-row fieldset)]))]))

(defmethod fields-error-class :bootstrap3-stacked [_]
  "problem has-error")

(defmethod render-form :bootstrap3-stacked [form-attrs fields opts]
  (render-bootstrap-form form-attrs fields "form-shell" opts))

;; Broken
#_(defmethod render-form :bootstrap3-horizontal [form-attrs fields opts]
   (render-bootstrap-form form-attrs fields "form-shell form-horizontal" opts))
