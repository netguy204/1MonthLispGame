(ns MonthGame.surface
  (:import (javax.swing JPanel JFrame JFileChooser
			JMenu JMenuItem)
	   (javax.swing.filechooser FileFilter FileView)
	   (java.awt.event ActionListener)))

(defn make-surface [draw-fn]
  (proxy [JPanel] []
    (paint [g] (draw-fn g this))))

(defn make-window [title & opts]
  (let [frame (JFrame. title)]
    (cond
     (some #{:close-on-exit} opts)
     (.setDefaultCloseOperation frame
				(JFrame/EXIT_ON_CLOSE)))
    frame))

(defn attach-mouse-adapter [panel adapter]
  (doto panel
    (.addMouseListener adapter)
    (.addMouseMotionListener adapter)
    (.addMouseWheelListener adapter)))

(defn- generate-menu-item
  ([name]
     (JMenu. name))

  ([name fn]
     (let [item (JMenuItem. name)]
       (.addActionListener item
	(proxy [ActionListener] []
	  (actionPerformed [e] (#(fn)))))
       item)))

(defn build-menu [specs menu]
  (doseq [subspecs specs]
    (let [[name spec] subspecs]
      (.add menu
	    (cond
	     (fn? spec) (generate-menu-item name spec)
	     true (build-menu spec (generate-menu-item name))))))
  menu)

(defn- split-ext [name]
  (let [regex #"([^\.]+)\.(.+)"]
    (if-let [[orig base ext] (re-matches regex name)]
      [base ext])))

(defn- file-ext [name]
  (if-let [[base ext] (split-ext name)]
    ext))

(defn- ext-matches [file extensions]
  (if-let [ext (file-ext (str file))]
    (some {ext true} extensions)))
  
(defn- make-file-filter [filter]
  (let [desc (first filter)
	extensions (rest filter)]
    (proxy [FileFilter] []
      (accept
       [f]
       (if (.isDirectory f) true
	   (if (ext-matches f extensions) true false)))

      (getDescription [] desc))))

(defn- make-file-namer [filters]
  (letfn
      [(get-match [file filters]
		  (if (empty? filters) nil
		      (let [filter (first filters)
			    desc (first filter)
			    extensions (rest filter)]
			(if (ext-matches file extensions) desc
			    (get-match file (rest filters))))))]
    #(get-match % filters)))

(defn- make-file-view [filters]
  (let [namer (make-file-namer filters)]
    (proxy [FileView] []
      (getTypeDescription
       [f] (namer f))

      (getIcon [f] nil)

      (getName [f] nil)

      (getDescription [f] nil)

      (isTraversable [f] nil))))

(defn- selector-base [action & filters]
  (let [dialog (JFileChooser.)]
    (doseq [filter filters]
      (.addChoosableFileFilter dialog (make-file-filter filter)))
    (.setFileView dialog (make-file-view filters))

    (let [result (action dialog)]
      (if (= result (JFileChooser/APPROVE_OPTION))
	(.. (.getSelectedFile dialog) (getAbsolutePath))
	nil))))

(defn open-selector [parent & filters]
  (apply selector-base #(.showOpenDialog % parent) filters))

(defn save-selector [parent & filters]
  (apply selector-base #(.showSaveDialog % parent) filters))
