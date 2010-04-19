(ns pl.danieljanus.sunflower
  (:gen-class)
  (:require [clojure.contrib.str-utils2 :as str])
  (:use (clojure.contrib def duck-streams seq-utils miglayout swing-utils)
        hiccup.core
        pl.danieljanus.tagsoup
        pl.danieljanus.iter)
  (:import (java.awt Component Dimension)
           (java.awt.event ActionListener MouseAdapter)
           (javax.swing DefaultListModel JButton JLabel JList JPanel JFrame JSeparator JFileChooser JScrollPane JSplitPane JProgressBar SwingConstants)
           (javax.swing.event ListSelectionListener)
           (org.xhtmlrenderer.simple XHTMLPanel FSScrollPane)
           (org.xhtmlrenderer.simple.extend XhtmlNamespaceHandler)
           (org.apache.commons.io FileUtils)
           ))

;; Logic

(defn strings
  "Returns a seq of all strings in parsed TREE."
  [tree]
  (letfn [(discardable? [x] (and (not (string? x)) (#{:style :script} (tag x))))]
    (if (string? tree)
      (list tree)
      (apply concat (map strings (remove discardable? (children tree)))))))

(defn any-contains? 
  [strs s]
  (some #(str/contains? % s) strs))

(defn find-subtree 
  [tree strs]
  (when-not (empty? strs)
    (loop [tree tree coords ()]
      (let [q (iter (for t in (when-not (string? tree) (children tree)))
                    (for i from 0)
                    (return [t i] if (every? #(any-contains? (strings t) %) strs)))]
        (if q
          (recur (first q) (cons (second q) coords))
          (reverse coords))))))

(defn extract-subtree
  [tree coords]
  (reduce #(nth (children %1) %2) tree coords))

;; UI

;; Helpers

(defn htmls [dir]
  (filter #(re-find #".html?$" (str %)) (file-seq dir)))
 
;; Component bag

(let [component-bag (atom {})]
     (defn add-component [id component]
       (when id (swap! component-bag assoc id component))
       component)
     (defn get-components []
       @component-bag)
     (defn get-component [id]
       (@component-bag id)))

(defmacro with-components [names & body]
  `(let ~(vec (interleave (map #(symbol (name %)) names)
                           (map (fn [x] `(get-component ~(keyword (name x)))) names)))
     ~@body))

;; Dialogs

(defnk choose-file [:mode :files]
  (let [chooser (JFileChooser.)]
    (doto chooser
      (.setFileSelectionMode (condp = mode 
                               :files JFileChooser/FILES_ONLY
                               :dirs JFileChooser/DIRECTORIES_ONLY
                               :both JFileChooser/FILES_AND_DIRECTORIES))
      (.showOpenDialog nil))
    (.getSelectedFile chooser)))

;; Events

(defmacro onclick [obj & body]
  `(.addActionListener ~obj (proxy [ActionListener] []
                            (actionPerformed [_#] ~@body))))

;; Widgets

(defn label
  "Returns a swing label"
  ([text] (JLabel. text))
  ([id text] (add-component id (label text))))

(defmacro button [id name & on-click]
  `(let [b# (JButton. ~name)]
     (onclick b# ~@on-click)
     (add-component ~id b#)))

(defn panel [id & rest]
  (add-component id (apply miglayout (JPanel.) rest)))

(defnk split-pane [id split w1 w2 :divider]
  (let [result (add-component id
                              (JSplitPane. (if (= split :horizontal)
                                             JSplitPane/HORIZONTAL_SPLIT
                                             JSplitPane/VERTICAL_SPLIT)
                                           w1 w2))]
    (when divider
      (.setDividerLocation result divider))
    result))

(defn kstr [k & strings]
  (keyword (apply str (name k) strings)))

(defn wizard [id & panels]
  (let [panels (vec panels)
        back-button (button (kstr id "-back") "< Back")
        next-button (button (kstr id "-next") "Next >")
        cancel (button (kstr id "-cancel") "Cancel" (System/exit 0))
        result (JPanel.)
        current (atom 0)
        options (into {} (map #(if (instance? Component %) 
                                 [% {}]
                                 [(first %) (apply hash-map (rest %))]) panels))
        panels (map #(if (instance? Component %) % (first %)) panels)
        _ (prn (vals options))
        update (fn [update-fn]
                 (let [old-panel (nth panels @current)
                       _ (swap! current update-fn)
                       panel (nth panels @current)]
                   (.remove result old-panel)
                   (.add result panel "cell 0 0, grow")
                   (when-let [f (:on-show (options panel))] (f))
                   (.revalidate result) 
                   (.repaint result)
                   (.setEnabled back-button (> @current 0))
                   (.setEnabled next-button (< @current (dec (count panels))))
                   (when-let [f (:after-show (options panel))] (f))))]
    (onclick back-button (update dec))
    (onclick next-button (update inc))
    (.setPreferredSize result (Dimension. 700 400))
    (update identity)
    (miglayout result
               :layout :nocache
               :row "[grow][][]"
               :column "[grow]"
               (first panels) "cell 0 0" :grow :wrap
               (JSeparator.) "cell 0 1" :grow :wrap
               back-button "cell 0 2" :align :right next-button "cell 0 2" cancel "cell 0 2" "gap unrelated")))

(defnk jlist [id :onselect nil]
  (let [model (add-component (kstr id "-model") (DefaultListModel.))
        result (add-component id (JList. model))]
    (when onselect
      (.addListSelectionListener result
                                 (proxy [ListSelectionListener] []
                                   (valueChanged [_]
                                                 (onselect (.getSelectedValues result))))))
    result))

(defn frame [id title content]
  (add-component id 
                 (doto (JFrame. title)
                   (.add content)
                   (.pack)
                   (.setVisible true))))

(defnk progress-bar [id :paint-string true]
  (doto (add-component id (JProgressBar.))
    (.setStringPainted paint-string)))

(defn set-html [xhtml text]
  (.setDocumentFromString xhtml text nil (XhtmlNamespaceHandler.)))

(defnk xhtml-panel [id :initial-text nil]
  (let [result (add-component id (XHTMLPanel.))]
    (when initial-text
      (set-html result (if (string? initial-text) initial-text (html initial-text))))
    result))

;; UI proper

(defn backup-dir [dir]
  (let [name (.getName dir)
        parent (.getParent dir)]
    (java.io.File. parent (str name ".backup"))))

(defmacro do-background [& body]
  `(send-off (agent nil) (fn [_#] ~@body)))

(defn strings-picked [strs]
  (when-not (empty? strs)
    (let [subtree-pos (find-subtree (get-component :parsed) strs)
          subtree (extract-subtree (get-component :parsed) subtree-pos)]
      (add-component :subtree-path subtree-pos)
      (set-html (get-component :xhtml) (html [:html subtree])))))

(def pick-text 
     [:html 
      [:p [:big "Please pick relevant strings."]]
      [:p "Sunflower has chosen a random document out of your collection. 
In the left panel are shown the contiguous strings in that document. 
Now you have to select some strings (press Ctrl to select multiple 
options), and Sunflower will extract the document part and show
it here.  When satisfied with the result, press Next."]])

(def welcome-text
     "<html><big>Welcome to Sunflower!</big><br><br>
Sunflower allows you to easily remove noise from<br>
a bunch of similarly-looking HTML files, leaving the pure<br>
text &mdash; the content you're actually interested in.<br><br>
Press Next to start.")

(defn save-extracted-tree
  "Serializes extracted tree to HTML and saves it to a file."
  [file tree]
  (spit file
        (html [:html
               [:head [:meta {:http-equiv "Content-Type"
                              :content "text/html; charset=utf-8"}]]
               [:body tree]])))

(defn operate []
  (with-components [selected-dir wizard-back wizard-cancel progress subtree-path errors-model operation]
    (let [files (htmls selected-dir)]
      (.setEnabled wizard-back false)
      (.setMaximum progress (count files))
      (.setText operation "Creating backup...")
      (do-background
        (FileUtils/copyDirectory selected-dir (backup-dir selected-dir))
        (doseq [file files]
          (do-swing
            (.setText operation (format "Processing %s..." file))
            (.setValue progress (inc (.getValue progress))))
          (let [out (-> file parse (extract-subtree subtree-path))]
            (if out
              (save-extracted-tree file out)
              (do
                (do-swing (.addElement errors-model (format "Extracted empty subtree from %s, deleting" file)))
                (.delete file)))))
        (do-swing
          (.setText operation "Finished! Press Finish to exit Sunflower.")
          (.setText wizard-cancel "Finish"))))))

(defn ui []
  (frame nil "Sunflower"
         (wizard :wizard
                 (panel nil
                        :layout "align 50% 50%"
                        (label welcome-text))
                 [(panel nil
                         :layout "align 50% 50%"
                         (label "Please select the directory containing your selection:") :wrap
                         (label :selected-dir-label "[No directory selected]") :split 
                         (button nil "Select..."
                                 (when-let [dir (choose-file :mode :dirs)]
                                   (with-components [wizard-next selected-dir-label]
                                     (add-component :selected-dir dir)
                                     (.setEnabled wizard-next true)
                                     (.setText selected-dir-label 
                                               (format "%s (%d files)" dir (count (htmls dir))))))))
                  :after-show #(with-components [wizard-next selected-dir]
                                 (.setEnabled wizard-next (not (not selected-dir))))]
                 [(split-pane nil :horizontal
                              (JScrollPane. (jlist :strings :onselect strings-picked))
                              (FSScrollPane. (xhtml-panel :xhtml :initial-text pick-text))
                              :divider 250)
                  :on-show #(with-components [strings-model selected-dir]
                              (.removeAllElements strings-model)
                              (let [p (-> selected-dir htmls first parse)]
                                (add-component :parsed p)
                                (doseq [x (strings p)]
                                  (.addElement strings-model x))))]
                 [(FSScrollPane. (xhtml-panel :xhtml-verify))
                  :on-show #(with-components [xhtml-verify selected-dir subtree-path]
                              (set-html xhtml-verify
                                        (html (-> selected-dir htmls second parse (extract-subtree subtree-path)))))]
                 [(panel nil
                         :row "[][][grow]"
                         :column "[grow]"
                         (label :operation "") :grow :wrap
                         (progress-bar :progress) :grow :wrap
                         (JScrollPane. (jlist :errors)) :grow)
                  :after-show operate])))

(defn -main []
  (ui))