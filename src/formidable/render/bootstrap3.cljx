(ns formidable.render.bootstrap3
  (:require [formidable.data :as data]
            [formidable.render :refer [render-form render-field
                                       render-problems
                                       get-hour+ampm format-minutes
                                       format-time time-range round
                                       build-opt-tag get-input-attrs
                                       get-field-label render-input-val]]
            [formidable.util :as fu]
            [clojure.string :as string]))

(defn render-default-input [field & [opts]]
  (let [attrs (get-input-attrs field [:type :name :id :class :value :autofocus
                                      :checked :disabled :href :style :src :size
                                      :readonly :tabindex :onchange :onclick
                                      :onfocus :onblur :placeholder :autofill
                                      :multiple :title])
        attrs (if (and (= :submit (:type attrs))
                       (empty? (:value attrs)))
                (dissoc attrs :value)
                (assoc attrs :value (render-input-val field)))
        attrs (assoc attrs :class (str (:class attrs) " form-control"))
        attrs (assoc attrs :type (name (or (:type attrs) :text)))]
    (list
      (when-let [prefix (:prefix opts)]
        [:span.input-prefix prefix])
      [:input attrs])))

(defmethod render-field :default
  ([field]
    (render-default-input field))
  ([field renderer]
    (render-field field)))

(defmethod render-field [:date-select :bootstrap3-stacked] [field _]
  (let [date (fu/normalize-date (:value field) nil (:timezone field))
        [year month day] (when date
                           (fu/get-year-month-day date))
        this-year (fu/get-this-year)
        year-start (:year-start field this-year)
        year-end (:year-end field (+ this-year 20))]
    [:span.date-select
     (render-field {:name (:name field)
                    :type :compound
                    :separator " "
                    :fields [{:type :select
                              :name "month"
                              :class "input-medium"
                              :value month
                              :options (cons ["" "Month"]
                                             (map vector
                                                  (range 1 13)
                                                  (fu/get-month-names)))}
                             {:type :select
                              :name "day"
                              :class "input-small"
                              :value day
                              :options (cons ["" "Day"]
                                             (map #(vector % %) (range 1 32)))}
                             {:type :select
                              :name "year"
                              :class "input-small"
                              :value year
                              :options (cons ["" "Year"]
                                             (map #(vector % %)
                                                  (range year-start (inc year-end))))}]}
                   :bootstrap3-stacked)]))

(defmethod render-field [:year-select :bootstrap3-stacked] [field _]
  (let [this-year (fu/get-this-year)
        start (:start field this-year)
        end (:end field (+ this-year 20))]
    [:div.year-select
     (render-field (assoc field
                          :class (str (:class field) " input-small")
                          :type :select
                          :options (range start (inc end)))
                   :bootstrap3-stacked)]))

(defmethod render-field [:month-select :bootstrap3-stacked] [field _]
  (let [opts (if (:numbers field)
               (range 1 13)
               (map vector
                    (range 1 13)
                    (fu/get-month-names)))]
    [:div.month-select
     (render-field (assoc field
                          :class (str (:class field) " input-medium")
                          :type :select
                          :options opts)
                   :bootstrap3-stacked)]))

(defmethod render-field [:compound :bootstrap3-stacked] [field _]
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

(defmethod render-field [:us-state :bootstrap3-stacked] [field _]
  (render-field (assoc field
                       :type :select
                       :options data/us-states)
                :bootstrap3-stacked))

(defmethod render-field [:select :bootstrap3-stacked] [field _]
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

(defn- render-time-select-multi [fname h m s step ampm? seconds?]
  (let [[h ampm] (get-hour+ampm h ampm?)]
    (list
      (render-field {:type :select
                     :name (str fname "[h]")
                     :class "input-small"
                     :value h
                     :first-option ["" "--"]
                     :options (if ampm? (range 1 13) (range 0 24))}
                    :bootstrap3-stacked)
      " "
      (render-field {:type :select
                     :name (str fname "[m]")
                     :class "input-small"
                     :value m
                     :first-option ["" "--"]
                     :options (map (juxt identity format-minutes)
                                   (range 0 60 step))}
                    :bootstrap3-stacked)
      (when seconds?
        (list
          " "
          (render-field {:type :select
                         :name (str fname "[s]")
                         :class "input-small"
                         :value s
                         :first-option ["" "--"]
                         :options (map (juxt identity format-minutes)
                                       (range 0 60 step))}
                        :bootstrap3-stacked)))
      (when ampm?
        (list
          " "
          (render-field {:type :select
                         :name (str fname "[ampm]")
                         :class "input-small"
                         :value ampm
                         :first-option ["" "--"]
                         :options ["am" "pm"]}
                        :bootstrap3-stacked))))))

(defn- render-time-select-single [field h m step ampm? start end]
  (let [start (or (fu/normalize-time start)
                  (fu/normalize-time {:h 0 :m 0}))
        end (or (fu/normalize-time end)
                (fu/normalize-time {:h 23 :m 59}))
        opts (for [[h m] (time-range (fu/get-hours-minutes-seconds start)
                                     (fu/get-hours-minutes-seconds end)
                                     step)]
               [(str h ":" (format-minutes m))
                (format-time h m ampm?)])]
    (render-field (assoc field
                         :type :select
                         :value (str h ":" (format-minutes m))
                         :options opts)
                  :bootstrap3-stacked)))

(defmethod render-field [:time-select :bootstrap3-stacked] [field _]
  (let [step (:step field 5)
        ampm? (:ampm field true)
        time (fu/normalize-time (:value field))
        [h m s] (when time
                  (fu/get-hours-minutes-seconds time))
        m (when m (round m step))
        s (when s (round s step))
        seconds? (:seconds field false)]
    [:span.time-select
     (if (:compact field)
       (render-time-select-single field h m step ampm? (:start field) (:end field))
       (render-time-select-multi (:name field) h m s step ampm? seconds?))]))

(defmethod render-field [:datetime-select :bootstrap3-stacked] [field _]
  [:span.datetime-select
   (render-field (assoc field :type :date-select)
                 :bootstrap3-stacked)
   " "
   (render-field (assoc field :type :time-select :compact false)
                 :bootstrap3-stacked)])

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
                field)]

    [:div {:id (fu/get-field-container-id field)
           :class (str (cond
                         checkbox? "checkbox "
                         inline? "form-inline "
                         :else     "form-group ")
                       (:div-class field)
                       (when (:problem field) " has-error problem " ))}
     (if (= :heading (:type field))
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
            [:div.note.help-inline (:note field)])))]))

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
         (render-problems problems fields)))
      (list
       (map (fn [field] (render-field field :bootstrap3-stacked))
            hidden-fields)
       (for [fieldset (group-fieldsets visible-fields)]
         [:fieldset {:class (str "fieldset-" (name (:name (first fieldset))))}
          (map render-bootstrap-row fieldset)]))]))

(defmethod render-form :bootstrap3-stacked [form-attrs fields opts]
  (render-bootstrap-form form-attrs fields "form-shell" opts))

;; Broken
#_(defmethod render-form :bootstrap3-horizontal [form-attrs fields opts]
   (render-bootstrap-form form-attrs fields "form-shell form-horizontal" opts))
