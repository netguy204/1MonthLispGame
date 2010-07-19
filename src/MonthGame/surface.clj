;; surface.clj
;; Turn based tank combat game
;;
;; Copyright 2010 Brian Taylor
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns MonthGame.surface
  (:use (MonthGame util))
  (:import (javax.swing JPanel JFrame JFileChooser
			JMenu JMenuItem JComboBox)
	   (javax.swing.filechooser FileFilter FileView)
	   (java.awt.event ActionListener)))

(defn make-surface [draw-fn]
  (proxy [JPanel] []
    (paint [#^Graphics2D g] (draw-fn g this))))

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

(defn ext-matches [file extensions]
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

(defn- record-for-file [file recs]
  (first (filter #(ext-matches file (:type %)) recs)))

(defn open-selector-then-invoke [frecs parent]
  (let [ftypes (map #(:type %) frecs)
	fname (apply open-selector parent ftypes)]
    (if (not (nil? fname))
      (let [selected-record (record-for-file fname frecs)]
	((:fn selected-record) fname)))))

(defn make-combo-box [spec]
  (let [strings (map #(:name %) spec)
	combo (JComboBox. (seq-to-string-array strings))
	funcs (zipmap strings (map #(:fn %) spec))
	listener (proxy [ActionListener] []
		   (actionPerformed 
		    [e] (let [str (.getSelectedItem combo)
			      fn (funcs str)]
			  (fn))))]
    (.addActionListener combo listener)
    combo))

