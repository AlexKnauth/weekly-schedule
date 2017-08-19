#lang agile

(require weekly-schedule/image
         (submod weekly-schedule/data example))

(weekly-schedule->image WEEKLY-SCHEDULE-EXAMPLE-1)

