using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Entity;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using nReddit.Models;

namespace nReddit.Controllers
{ 
    public class SubmissionController : Controller
    {
        private NRedditEntities db = new NRedditEntities();

        //
        // GET: /Submission/Details/5

        public ViewResult Details(int id)
        {
            Submission submission = db.Submissions.Find(id);
            return View(submission);
        }

        //
        // GET: /Submission/Create

        public ActionResult Create()
        {
            return View();
        } 

        //
        // POST: /Submission/Create

        [HttpPost]
        public ActionResult Create(Submission submission)
        {
            if (ModelState.IsValid)
            {
                int subredditID = (int)Session["subredditID"];
                submission.SubredditID = subredditID;
                Subreddit subreddit =
                    db.Subreddits.Include("Submissions").Single(s => s.SubredditID == subredditID);
                submission.Subreddit = subreddit;
                db.Submissions.Add(submission);
                subreddit.Submissions.Add(submission);
                db.Entry(subreddit).State = EntityState.Modified;
                db.SaveChanges();
                return RedirectToAction("Details", new { id = submission.SubmissionID });  
            }
            return View(submission);
        }
        
        //
        // GET: /Submission/Edit/5
 
        public ActionResult Edit(int id)
        {
            Submission submission = db.Submissions.Find(id);
            return View(submission);
        }

        //
        // POST: /Submission/Edit/5

        [HttpPost]
        public ActionResult Edit(Submission submission)
        {
            if (ModelState.IsValid)
            {
                db.Entry(submission).State = EntityState.Modified;
                db.SaveChanges();
                return RedirectToAction("Details", new { id = submission.SubmissionID });
            }
            return View(submission);
        }

        //
        // GET: /Submission/Delete/5
 
        public ActionResult Delete(int id)
        {
            Submission submission = db.Submissions.Find(id);
            return View(submission);
        }

        //
        // POST: /Submission/Delete/5

        [HttpPost, ActionName("Delete")]
        public ActionResult DeleteConfirmed(int id)
        {            
            Submission submission = db.Submissions.Find(id);
            db.Submissions.Remove(submission);
            db.SaveChanges();
            return RedirectToAction("Details", "Subreddit", new { id = submission.SubredditID });
        }

        protected override void Dispose(bool disposing)
        {
            db.Dispose();
            base.Dispose(disposing);
        }
    }
}