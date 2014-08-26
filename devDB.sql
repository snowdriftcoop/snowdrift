--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: log_doc_event_trigger(); Type: FUNCTION; Schema: public; Owner: snowdrift_development
--

CREATE FUNCTION log_doc_event_trigger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
        IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN
            INSERT INTO doc_event (time, doc, blessed_version) SELECT now(), NEW.id, NEW.current_version;
            RETURN NEW;
        END IF;
        RETURN NULL;
    END;
$$;


ALTER FUNCTION public.log_doc_event_trigger() OWNER TO snowdrift_development;

--
-- Name: log_role_event_trigger(); Type: FUNCTION; Schema: public; Owner: snowdrift_development
--

CREATE FUNCTION log_role_event_trigger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
        IF (TG_OP = 'DELETE') THEN
            INSERT INTO role_event (time, "user", role, project, added) SELECT now(), OLD."user", OLD.role, OLD.project, 'f';
            RETURN OLD;
        ELSIF (TG_OP = 'INSERT') THEN
            INSERT INTO role_event (time, "user", role, project, added) SELECT now(), NEW."user", NEW.role, NEW.project, 't';
            RETURN NEW;
        END IF;
        RETURN NULL;
    END;
$$;


ALTER FUNCTION public.log_role_event_trigger() OWNER TO snowdrift_development;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: account; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE account (
    id integer NOT NULL,
    balance bigint NOT NULL
);


ALTER TABLE public.account OWNER TO snowdrift_development;

--
-- Name: account_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE account_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.account_id_seq OWNER TO snowdrift_development;

--
-- Name: account_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE account_id_seq OWNED BY account.id;


--
-- Name: build; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE build (
    id integer NOT NULL,
    boot_time timestamp without time zone NOT NULL,
    base character varying NOT NULL,
    diff character varying NOT NULL
);


ALTER TABLE public.build OWNER TO snowdrift_development;

--
-- Name: build_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE build_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.build_id_seq OWNER TO snowdrift_development;

--
-- Name: build_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE build_id_seq OWNED BY build.id;


--
-- Name: comment; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE comment (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    approved_ts timestamp without time zone,
    approved_by bigint,
    parent bigint,
    "user" bigint NOT NULL,
    text character varying NOT NULL,
    depth bigint NOT NULL,
    discussion bigint NOT NULL
);


ALTER TABLE public.comment OWNER TO snowdrift_development;

--
-- Name: comment_ancestor; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE comment_ancestor (
    id integer NOT NULL,
    comment bigint NOT NULL,
    ancestor bigint NOT NULL
);


ALTER TABLE public.comment_ancestor OWNER TO snowdrift_development;

--
-- Name: comment_ancestor_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE comment_ancestor_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_ancestor_id_seq OWNER TO snowdrift_development;

--
-- Name: comment_ancestor_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE comment_ancestor_id_seq OWNED BY comment_ancestor.id;


--
-- Name: comment_closure; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE comment_closure (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    reason character varying NOT NULL,
    comment bigint NOT NULL,
    closed_by bigint NOT NULL,
    type character varying NOT NULL
);


ALTER TABLE public.comment_closure OWNER TO snowdrift_development;

--
-- Name: comment_flagging; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE comment_flagging (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    flagger bigint NOT NULL,
    comment bigint NOT NULL,
    message character varying
);


ALTER TABLE public.comment_flagging OWNER TO snowdrift_development;

--
-- Name: comment_flagging_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE comment_flagging_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_flagging_id_seq OWNER TO snowdrift_development;

--
-- Name: comment_flagging_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE comment_flagging_id_seq OWNED BY comment_flagging.id;


--
-- Name: comment_flagging_reason; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE comment_flagging_reason (
    id integer NOT NULL,
    flagging bigint NOT NULL,
    reason character varying NOT NULL
);


ALTER TABLE public.comment_flagging_reason OWNER TO snowdrift_development;

--
-- Name: comment_flagging_reason_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE comment_flagging_reason_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_flagging_reason_id_seq OWNER TO snowdrift_development;

--
-- Name: comment_flagging_reason_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE comment_flagging_reason_id_seq OWNED BY comment_flagging_reason.id;


--
-- Name: comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_id_seq OWNER TO snowdrift_development;

--
-- Name: comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE comment_id_seq OWNED BY comment.id;


--
-- Name: comment_rethread; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE comment_rethread (
    id integer NOT NULL,
    rethread bigint NOT NULL,
    old_comment bigint NOT NULL,
    new_comment bigint NOT NULL
);


ALTER TABLE public.comment_rethread OWNER TO snowdrift_development;

--
-- Name: comment_rethread_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE comment_rethread_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_rethread_id_seq OWNER TO snowdrift_development;

--
-- Name: comment_rethread_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE comment_rethread_id_seq OWNED BY comment_rethread.id;


--
-- Name: comment_retraction; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE comment_retraction (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    reason character varying NOT NULL,
    comment bigint NOT NULL
);


ALTER TABLE public.comment_retraction OWNER TO snowdrift_development;

--
-- Name: comment_retraction_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE comment_retraction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_retraction_id_seq OWNER TO snowdrift_development;

--
-- Name: comment_retraction_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE comment_retraction_id_seq OWNED BY comment_closure.id;


--
-- Name: comment_retraction_id_seq1; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE comment_retraction_id_seq1
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_retraction_id_seq1 OWNER TO snowdrift_development;

--
-- Name: comment_retraction_id_seq1; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE comment_retraction_id_seq1 OWNED BY comment_retraction.id;


--
-- Name: comment_tag; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE comment_tag (
    id integer NOT NULL,
    comment bigint NOT NULL,
    tag bigint NOT NULL,
    "user" bigint NOT NULL,
    count bigint DEFAULT 1 NOT NULL
);


ALTER TABLE public.comment_tag OWNER TO snowdrift_development;

--
-- Name: comment_tag_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE comment_tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_tag_id_seq OWNER TO snowdrift_development;

--
-- Name: comment_tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE comment_tag_id_seq OWNED BY comment_tag.id;


--
-- Name: committee_user; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE committee_user (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL
);


ALTER TABLE public.committee_user OWNER TO snowdrift_development;

--
-- Name: committee_user_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE committee_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.committee_user_id_seq OWNER TO snowdrift_development;

--
-- Name: committee_user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE committee_user_id_seq OWNED BY committee_user.id;


--
-- Name: database_version; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE database_version (
    id integer NOT NULL,
    last_migration bigint NOT NULL
);


ALTER TABLE public.database_version OWNER TO snowdrift_development;

--
-- Name: database_version_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE database_version_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.database_version_id_seq OWNER TO snowdrift_development;

--
-- Name: database_version_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE database_version_id_seq OWNED BY database_version.id;


--
-- Name: default_tag_color; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE default_tag_color (
    id integer NOT NULL,
    tag bigint NOT NULL,
    color bigint NOT NULL
);


ALTER TABLE public.default_tag_color OWNER TO snowdrift_development;

--
-- Name: default_tag_color_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE default_tag_color_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.default_tag_color_id_seq OWNER TO snowdrift_development;

--
-- Name: default_tag_color_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE default_tag_color_id_seq OWNED BY default_tag_color.id;


--
-- Name: discussion; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE discussion (
    id integer NOT NULL,
    nothing bigint NOT NULL
);


ALTER TABLE public.discussion OWNER TO snowdrift_development;

--
-- Name: discussion_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE discussion_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.discussion_id_seq OWNER TO snowdrift_development;

--
-- Name: discussion_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE discussion_id_seq OWNED BY discussion.id;


--
-- Name: doc; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE doc (
    id integer NOT NULL,
    name character varying NOT NULL,
    current_version bigint NOT NULL
);


ALTER TABLE public.doc OWNER TO snowdrift_development;

--
-- Name: doc_event; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE doc_event (
    id integer NOT NULL,
    "time" timestamp without time zone NOT NULL,
    doc bigint NOT NULL,
    blessed_version bigint NOT NULL
);


ALTER TABLE public.doc_event OWNER TO snowdrift_development;

--
-- Name: doc_event_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE doc_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.doc_event_id_seq OWNER TO snowdrift_development;

--
-- Name: doc_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE doc_event_id_seq OWNED BY doc_event.id;


--
-- Name: doc_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE doc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.doc_id_seq OWNER TO snowdrift_development;

--
-- Name: doc_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE doc_id_seq OWNED BY doc.id;


--
-- Name: event_comment_pending; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE event_comment_pending (
    id integer NOT NULL,
    comment bigint NOT NULL,
    ts timestamp without time zone NOT NULL
);


ALTER TABLE public.event_comment_pending OWNER TO snowdrift_development;

--
-- Name: event_comment_pending_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE event_comment_pending_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_comment_pending_id_seq OWNER TO snowdrift_development;

--
-- Name: event_comment_pending_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE event_comment_pending_id_seq OWNED BY event_comment_pending.id;


--
-- Name: event_comment_posted; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE event_comment_posted (
    id integer NOT NULL,
    comment bigint NOT NULL,
    ts timestamp without time zone NOT NULL
);


ALTER TABLE public.event_comment_posted OWNER TO snowdrift_development;

--
-- Name: event_comment_posted_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE event_comment_posted_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_comment_posted_id_seq OWNER TO snowdrift_development;

--
-- Name: event_comment_posted_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE event_comment_posted_id_seq OWNED BY event_comment_posted.id;


--
-- Name: event_deleted_pledge; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE event_deleted_pledge (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL,
    shares bigint NOT NULL
);


ALTER TABLE public.event_deleted_pledge OWNER TO snowdrift_development;

--
-- Name: event_deleted_pledge_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE event_deleted_pledge_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_deleted_pledge_id_seq OWNER TO snowdrift_development;

--
-- Name: event_deleted_pledge_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE event_deleted_pledge_id_seq OWNED BY event_deleted_pledge.id;


--
-- Name: event_new_pledge; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE event_new_pledge (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    shares_pledged bigint NOT NULL
);


ALTER TABLE public.event_new_pledge OWNER TO snowdrift_development;

--
-- Name: event_new_pledge_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE event_new_pledge_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_new_pledge_id_seq OWNER TO snowdrift_development;

--
-- Name: event_new_pledge_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE event_new_pledge_id_seq OWNED BY event_new_pledge.id;


--
-- Name: event_notification_sent; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE event_notification_sent (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    notification bigint NOT NULL
);


ALTER TABLE public.event_notification_sent OWNER TO snowdrift_development;

--
-- Name: event_notification_sent_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE event_notification_sent_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_notification_sent_id_seq OWNER TO snowdrift_development;

--
-- Name: event_notification_sent_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE event_notification_sent_id_seq OWNED BY event_notification_sent.id;


--
-- Name: event_updated_pledge; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE event_updated_pledge (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    old_shares bigint NOT NULL,
    shares_pledged bigint NOT NULL
);


ALTER TABLE public.event_updated_pledge OWNER TO snowdrift_development;

--
-- Name: event_updated_pledge_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE event_updated_pledge_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_updated_pledge_id_seq OWNER TO snowdrift_development;

--
-- Name: event_updated_pledge_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE event_updated_pledge_id_seq OWNED BY event_updated_pledge.id;


--
-- Name: event_wiki_edit; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE event_wiki_edit (
    id integer NOT NULL,
    wiki_edit bigint NOT NULL,
    ts timestamp without time zone NOT NULL
);


ALTER TABLE public.event_wiki_edit OWNER TO snowdrift_development;

--
-- Name: event_wiki_edit_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE event_wiki_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_wiki_edit_id_seq OWNER TO snowdrift_development;

--
-- Name: event_wiki_edit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE event_wiki_edit_id_seq OWNED BY event_wiki_edit.id;


--
-- Name: event_wiki_page; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE event_wiki_page (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    wiki_page bigint NOT NULL
);


ALTER TABLE public.event_wiki_page OWNER TO snowdrift_development;

--
-- Name: event_wiki_page_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE event_wiki_page_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_wiki_page_id_seq OWNER TO snowdrift_development;

--
-- Name: event_wiki_page_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE event_wiki_page_id_seq OWNED BY event_wiki_page.id;


--
-- Name: interest; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE interest (
    id integer NOT NULL,
    description character varying NOT NULL
);


ALTER TABLE public.interest OWNER TO snowdrift_development;

--
-- Name: interest_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE interest_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.interest_id_seq OWNER TO snowdrift_development;

--
-- Name: interest_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE interest_id_seq OWNED BY interest.id;


--
-- Name: invite; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE invite (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    project bigint NOT NULL,
    code character varying NOT NULL,
    "user" bigint NOT NULL,
    role character varying NOT NULL,
    tag character varying NOT NULL,
    redeemed boolean NOT NULL,
    redeemed_ts timestamp without time zone,
    redeemed_by bigint
);


ALTER TABLE public.invite OWNER TO snowdrift_development;

--
-- Name: invite_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE invite_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.invite_id_seq OWNER TO snowdrift_development;

--
-- Name: invite_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE invite_id_seq OWNED BY invite.id;


--
-- Name: manual_establishment; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE manual_establishment (
    id integer NOT NULL,
    established_user bigint NOT NULL,
    establishing_user bigint NOT NULL
);


ALTER TABLE public.manual_establishment OWNER TO snowdrift_development;

--
-- Name: manual_establishment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE manual_establishment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.manual_establishment_id_seq OWNER TO snowdrift_development;

--
-- Name: manual_establishment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE manual_establishment_id_seq OWNED BY manual_establishment.id;


--
-- Name: notification; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE notification (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    type character varying NOT NULL,
    "to" bigint NOT NULL,
    project bigint,
    content character varying NOT NULL,
    archived boolean NOT NULL
);


ALTER TABLE public.notification OWNER TO snowdrift_development;

--
-- Name: notification_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE notification_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.notification_id_seq OWNER TO snowdrift_development;

--
-- Name: notification_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE notification_id_seq OWNED BY notification.id;


--
-- Name: payday; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE payday (
    id integer NOT NULL,
    date timestamp without time zone NOT NULL
);


ALTER TABLE public.payday OWNER TO snowdrift_development;

--
-- Name: payday_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE payday_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.payday_id_seq OWNER TO snowdrift_development;

--
-- Name: payday_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE payday_id_seq OWNED BY payday.id;


--
-- Name: pledge; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE pledge (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL,
    shares bigint NOT NULL,
    funded_shares bigint NOT NULL,
    created_ts timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.pledge OWNER TO snowdrift_development;

--
-- Name: pledge_form_rendered; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE pledge_form_rendered (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    "order" character varying NOT NULL,
    project bigint NOT NULL,
    "user" bigint
);


ALTER TABLE public.pledge_form_rendered OWNER TO snowdrift_development;

--
-- Name: pledge_form_rendered_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE pledge_form_rendered_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.pledge_form_rendered_id_seq OWNER TO snowdrift_development;

--
-- Name: pledge_form_rendered_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE pledge_form_rendered_id_seq OWNED BY pledge_form_rendered.id;


--
-- Name: pledge_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE pledge_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.pledge_id_seq OWNER TO snowdrift_development;

--
-- Name: pledge_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE pledge_id_seq OWNED BY pledge.id;


--
-- Name: project; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    name character varying NOT NULL,
    handle character varying NOT NULL,
    description character varying NOT NULL,
    account bigint NOT NULL,
    share_value bigint NOT NULL,
    last_payday bigint,
    github_repo character varying,
    discussion bigint DEFAULT nextval('discussion_id_seq'::regclass) NOT NULL
);


ALTER TABLE public.project OWNER TO snowdrift_development;

--
-- Name: project_blog; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project_blog (
    id integer NOT NULL,
    "time" timestamp without time zone NOT NULL,
    title character varying NOT NULL,
    "user" bigint NOT NULL,
    top_content character varying NOT NULL,
    project bigint NOT NULL,
    bottom_content character varying,
    discussion bigint NOT NULL,
    handle character varying NOT NULL
);


ALTER TABLE public.project_blog OWNER TO snowdrift_development;

--
-- Name: project_blog_comment; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project_blog_comment (
    id integer NOT NULL,
    comment bigint NOT NULL,
    blog bigint NOT NULL
);


ALTER TABLE public.project_blog_comment OWNER TO snowdrift_development;

--
-- Name: project_blog_comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_blog_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_blog_comment_id_seq OWNER TO snowdrift_development;

--
-- Name: project_blog_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_blog_comment_id_seq OWNED BY project_blog_comment.id;


--
-- Name: project_blog_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_blog_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_blog_id_seq OWNER TO snowdrift_development;

--
-- Name: project_blog_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_blog_id_seq OWNED BY project_blog.id;


--
-- Name: project_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_id_seq OWNER TO snowdrift_development;

--
-- Name: project_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_id_seq OWNED BY project.id;


--
-- Name: project_last_update; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project_last_update (
    id integer NOT NULL,
    project bigint NOT NULL,
    update bigint NOT NULL
);


ALTER TABLE public.project_last_update OWNER TO snowdrift_development;

--
-- Name: project_last_update_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_last_update_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_last_update_id_seq OWNER TO snowdrift_development;

--
-- Name: project_last_update_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_last_update_id_seq OWNED BY project_last_update.id;


--
-- Name: project_tag; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project_tag (
    id integer NOT NULL,
    project bigint NOT NULL,
    tag bigint NOT NULL
);


ALTER TABLE public.project_tag OWNER TO snowdrift_development;

--
-- Name: project_tag_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_tag_id_seq OWNER TO snowdrift_development;

--
-- Name: project_tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_tag_id_seq OWNED BY project_tag.id;


--
-- Name: project_update; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project_update (
    id integer NOT NULL,
    updated_ts timestamp without time zone NOT NULL,
    project bigint NOT NULL,
    author bigint NOT NULL,
    description character varying NOT NULL
);


ALTER TABLE public.project_update OWNER TO snowdrift_development;

--
-- Name: project_update_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_update_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_update_id_seq OWNER TO snowdrift_development;

--
-- Name: project_update_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_update_id_seq OWNED BY project_update.id;


--
-- Name: project_user_role; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project_user_role (
    id integer NOT NULL,
    project bigint NOT NULL,
    "user" bigint NOT NULL,
    role character varying NOT NULL
);


ALTER TABLE public.project_user_role OWNER TO snowdrift_development;

--
-- Name: project_user_role_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_user_role_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_user_role_id_seq OWNER TO snowdrift_development;

--
-- Name: project_user_role_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_user_role_id_seq OWNED BY project_user_role.id;


--
-- Name: rethread; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE rethread (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    moderator bigint NOT NULL,
    old_comment bigint NOT NULL,
    reason character varying NOT NULL
);


ALTER TABLE public.rethread OWNER TO snowdrift_development;

--
-- Name: rethread_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE rethread_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.rethread_id_seq OWNER TO snowdrift_development;

--
-- Name: rethread_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE rethread_id_seq OWNED BY rethread.id;


--
-- Name: role_event; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE role_event (
    id integer NOT NULL,
    "time" timestamp without time zone NOT NULL,
    "user" bigint NOT NULL,
    role character varying NOT NULL,
    project bigint NOT NULL,
    added boolean NOT NULL
);


ALTER TABLE public.role_event OWNER TO snowdrift_development;

--
-- Name: role_event_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE role_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.role_event_id_seq OWNER TO snowdrift_development;

--
-- Name: role_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE role_event_id_seq OWNED BY role_event.id;


--
-- Name: shares_pledged; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE shares_pledged (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL,
    shares bigint NOT NULL,
    render bigint NOT NULL
);


ALTER TABLE public.shares_pledged OWNER TO snowdrift_development;

--
-- Name: shares_pledged_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE shares_pledged_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.shares_pledged_id_seq OWNER TO snowdrift_development;

--
-- Name: shares_pledged_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE shares_pledged_id_seq OWNED BY shares_pledged.id;


--
-- Name: tag; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE tag (
    id integer NOT NULL,
    name character varying NOT NULL
);


ALTER TABLE public.tag OWNER TO snowdrift_development;

--
-- Name: tag_color; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE tag_color (
    id integer NOT NULL,
    tag bigint NOT NULL,
    "user" bigint NOT NULL,
    color bigint NOT NULL
);


ALTER TABLE public.tag_color OWNER TO snowdrift_development;

--
-- Name: tag_color_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE tag_color_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.tag_color_id_seq OWNER TO snowdrift_development;

--
-- Name: tag_color_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE tag_color_id_seq OWNED BY tag_color.id;


--
-- Name: tag_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.tag_id_seq OWNER TO snowdrift_development;

--
-- Name: tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE tag_id_seq OWNED BY tag.id;


--
-- Name: ticket; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE ticket (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    name character varying NOT NULL,
    comment bigint NOT NULL,
    updated_ts timestamp without time zone NOT NULL
);


ALTER TABLE public.ticket OWNER TO snowdrift_development;

--
-- Name: ticket_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE ticket_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ticket_id_seq OWNER TO snowdrift_development;

--
-- Name: ticket_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE ticket_id_seq OWNED BY ticket.id;


--
-- Name: transaction; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE transaction (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    credit bigint,
    debit bigint,
    amount bigint NOT NULL,
    reason character varying NOT NULL,
    info character varying,
    payday bigint
);


ALTER TABLE public.transaction OWNER TO snowdrift_development;

--
-- Name: transaction_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE transaction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.transaction_id_seq OWNER TO snowdrift_development;

--
-- Name: transaction_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE transaction_id_seq OWNED BY transaction.id;


--
-- Name: unapproved_comment_notification; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE unapproved_comment_notification (
    id integer NOT NULL,
    comment bigint NOT NULL,
    notification bigint NOT NULL
);


ALTER TABLE public.unapproved_comment_notification OWNER TO snowdrift_development;

--
-- Name: unapproved_comment_notification_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE unapproved_comment_notification_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.unapproved_comment_notification_id_seq OWNER TO snowdrift_development;

--
-- Name: unapproved_comment_notification_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE unapproved_comment_notification_id_seq OWNED BY unapproved_comment_notification.id;


--
-- Name: user; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE "user" (
    id integer NOT NULL,
    ident character varying NOT NULL,
    hash character varying,
    salt character varying,
    name character varying,
    account bigint NOT NULL,
    avatar character varying,
    blurb character varying,
    statement character varying,
    irc_nick character varying,
    read_notifications timestamp without time zone DEFAULT now() NOT NULL,
    read_applications timestamp without time zone DEFAULT now() NOT NULL,
    created_ts timestamp without time zone,
    established character varying DEFAULT 'EstUnestablished'::character varying NOT NULL,
    discussion bigint DEFAULT nextval('discussion_id_seq'::regclass) NOT NULL
);


ALTER TABLE public."user" OWNER TO snowdrift_development;

--
-- Name: user_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_id_seq OWNER TO snowdrift_development;

--
-- Name: user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE user_id_seq OWNED BY "user".id;


--
-- Name: user_message_pref; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE user_message_pref (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    type character varying NOT NULL,
    delivery character varying NOT NULL
);


ALTER TABLE public.user_message_pref OWNER TO snowdrift_development;

--
-- Name: user_message_pref_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE user_message_pref_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_message_pref_id_seq OWNER TO snowdrift_development;

--
-- Name: user_message_pref_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE user_message_pref_id_seq OWNED BY user_message_pref.id;


--
-- Name: user_notification_pref; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE user_notification_pref (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    type character varying NOT NULL,
    delivery character varying NOT NULL
);


ALTER TABLE public.user_notification_pref OWNER TO snowdrift_development;

--
-- Name: user_notification_pref_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE user_notification_pref_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_notification_pref_id_seq OWNER TO snowdrift_development;

--
-- Name: user_notification_pref_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE user_notification_pref_id_seq OWNED BY user_notification_pref.id;


--
-- Name: user_setting; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE user_setting (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    setting character varying NOT NULL,
    value character varying NOT NULL
);


ALTER TABLE public.user_setting OWNER TO snowdrift_development;

--
-- Name: user_setting_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE user_setting_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_setting_id_seq OWNER TO snowdrift_development;

--
-- Name: user_setting_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE user_setting_id_seq OWNED BY user_setting.id;


--
-- Name: user_watching_project; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE user_watching_project (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL
);


ALTER TABLE public.user_watching_project OWNER TO snowdrift_development;

--
-- Name: user_watching_project_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE user_watching_project_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_watching_project_id_seq OWNER TO snowdrift_development;

--
-- Name: user_watching_project_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE user_watching_project_id_seq OWNED BY user_watching_project.id;


--
-- Name: view_comment; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE view_comment (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    comment bigint NOT NULL
);


ALTER TABLE public.view_comment OWNER TO snowdrift_development;

--
-- Name: view_comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE view_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.view_comment_id_seq OWNER TO snowdrift_development;

--
-- Name: view_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE view_comment_id_seq OWNED BY view_comment.id;


--
-- Name: view_time; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE view_time (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL,
    type character varying NOT NULL,
    "time" timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.view_time OWNER TO snowdrift_development;

--
-- Name: view_time_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE view_time_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.view_time_id_seq OWNER TO snowdrift_development;

--
-- Name: view_time_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE view_time_id_seq OWNED BY view_time.id;


--
-- Name: view_wiki_edit; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE view_wiki_edit (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    edit bigint NOT NULL
);


ALTER TABLE public.view_wiki_edit OWNER TO snowdrift_development;

--
-- Name: view_wiki_edit_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE view_wiki_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.view_wiki_edit_id_seq OWNER TO snowdrift_development;

--
-- Name: view_wiki_edit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE view_wiki_edit_id_seq OWNED BY view_wiki_edit.id;


--
-- Name: volunteer_application; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE volunteer_application (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    project bigint NOT NULL,
    "user" bigint NOT NULL,
    name character varying NOT NULL,
    email character varying NOT NULL,
    other_contact_info character varying,
    website character varying,
    location character varying,
    experience character varying,
    comments character varying
);


ALTER TABLE public.volunteer_application OWNER TO snowdrift_development;

--
-- Name: volunteer_application_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE volunteer_application_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.volunteer_application_id_seq OWNER TO snowdrift_development;

--
-- Name: volunteer_application_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE volunteer_application_id_seq OWNED BY volunteer_application.id;


--
-- Name: volunteer_interest; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE volunteer_interest (
    id integer NOT NULL,
    volunteer bigint NOT NULL,
    interest bigint NOT NULL
);


ALTER TABLE public.volunteer_interest OWNER TO snowdrift_development;

--
-- Name: volunteer_interest_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE volunteer_interest_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.volunteer_interest_id_seq OWNER TO snowdrift_development;

--
-- Name: volunteer_interest_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE volunteer_interest_id_seq OWNED BY volunteer_interest.id;


--
-- Name: wiki_edit; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE wiki_edit (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    "user" bigint NOT NULL,
    page bigint NOT NULL,
    content character varying NOT NULL,
    comment character varying
);


ALTER TABLE public.wiki_edit OWNER TO snowdrift_development;

--
-- Name: wiki_edit_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE wiki_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_edit_id_seq OWNER TO snowdrift_development;

--
-- Name: wiki_edit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE wiki_edit_id_seq OWNED BY wiki_edit.id;


--
-- Name: wiki_last_edit; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE wiki_last_edit (
    id integer NOT NULL,
    page bigint NOT NULL,
    edit bigint NOT NULL
);


ALTER TABLE public.wiki_last_edit OWNER TO snowdrift_development;

--
-- Name: wiki_last_edit_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE wiki_last_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_last_edit_id_seq OWNER TO snowdrift_development;

--
-- Name: wiki_last_edit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE wiki_last_edit_id_seq OWNED BY wiki_last_edit.id;


--
-- Name: wiki_page; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE wiki_page (
    id integer NOT NULL,
    target character varying NOT NULL,
    project bigint NOT NULL,
    content character varying NOT NULL,
    permission_level character varying NOT NULL,
    discussion bigint NOT NULL,
    created_ts timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.wiki_page OWNER TO snowdrift_development;

--
-- Name: wiki_page_comment; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE wiki_page_comment (
    id integer NOT NULL,
    comment bigint NOT NULL,
    page bigint NOT NULL
);


ALTER TABLE public.wiki_page_comment OWNER TO snowdrift_development;

--
-- Name: wiki_page_comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE wiki_page_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_page_comment_id_seq OWNER TO snowdrift_development;

--
-- Name: wiki_page_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE wiki_page_comment_id_seq OWNED BY wiki_page_comment.id;


--
-- Name: wiki_page_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE wiki_page_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_page_id_seq OWNER TO snowdrift_development;

--
-- Name: wiki_page_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE wiki_page_id_seq OWNED BY wiki_page.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY account ALTER COLUMN id SET DEFAULT nextval('account_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY build ALTER COLUMN id SET DEFAULT nextval('build_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment ALTER COLUMN id SET DEFAULT nextval('comment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_ancestor ALTER COLUMN id SET DEFAULT nextval('comment_ancestor_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_closure ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_flagging ALTER COLUMN id SET DEFAULT nextval('comment_flagging_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_flagging_reason ALTER COLUMN id SET DEFAULT nextval('comment_flagging_reason_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_rethread ALTER COLUMN id SET DEFAULT nextval('comment_rethread_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_retraction ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq1'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_tag ALTER COLUMN id SET DEFAULT nextval('comment_tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY committee_user ALTER COLUMN id SET DEFAULT nextval('committee_user_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY database_version ALTER COLUMN id SET DEFAULT nextval('database_version_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY default_tag_color ALTER COLUMN id SET DEFAULT nextval('default_tag_color_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY discussion ALTER COLUMN id SET DEFAULT nextval('discussion_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY doc ALTER COLUMN id SET DEFAULT nextval('doc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY doc_event ALTER COLUMN id SET DEFAULT nextval('doc_event_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_comment_pending ALTER COLUMN id SET DEFAULT nextval('event_comment_pending_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_comment_posted ALTER COLUMN id SET DEFAULT nextval('event_comment_posted_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_deleted_pledge ALTER COLUMN id SET DEFAULT nextval('event_deleted_pledge_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_new_pledge ALTER COLUMN id SET DEFAULT nextval('event_new_pledge_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_notification_sent ALTER COLUMN id SET DEFAULT nextval('event_notification_sent_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_updated_pledge ALTER COLUMN id SET DEFAULT nextval('event_updated_pledge_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_wiki_edit ALTER COLUMN id SET DEFAULT nextval('event_wiki_edit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_wiki_page ALTER COLUMN id SET DEFAULT nextval('event_wiki_page_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY interest ALTER COLUMN id SET DEFAULT nextval('interest_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY invite ALTER COLUMN id SET DEFAULT nextval('invite_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY manual_establishment ALTER COLUMN id SET DEFAULT nextval('manual_establishment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY notification ALTER COLUMN id SET DEFAULT nextval('notification_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY payday ALTER COLUMN id SET DEFAULT nextval('payday_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY pledge ALTER COLUMN id SET DEFAULT nextval('pledge_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY pledge_form_rendered ALTER COLUMN id SET DEFAULT nextval('pledge_form_rendered_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project ALTER COLUMN id SET DEFAULT nextval('project_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_blog ALTER COLUMN id SET DEFAULT nextval('project_blog_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_blog_comment ALTER COLUMN id SET DEFAULT nextval('project_blog_comment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_last_update ALTER COLUMN id SET DEFAULT nextval('project_last_update_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_tag ALTER COLUMN id SET DEFAULT nextval('project_tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_update ALTER COLUMN id SET DEFAULT nextval('project_update_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_user_role ALTER COLUMN id SET DEFAULT nextval('project_user_role_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY rethread ALTER COLUMN id SET DEFAULT nextval('rethread_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY role_event ALTER COLUMN id SET DEFAULT nextval('role_event_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY shares_pledged ALTER COLUMN id SET DEFAULT nextval('shares_pledged_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY tag ALTER COLUMN id SET DEFAULT nextval('tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY tag_color ALTER COLUMN id SET DEFAULT nextval('tag_color_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY ticket ALTER COLUMN id SET DEFAULT nextval('ticket_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY transaction ALTER COLUMN id SET DEFAULT nextval('transaction_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY unapproved_comment_notification ALTER COLUMN id SET DEFAULT nextval('unapproved_comment_notification_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY "user" ALTER COLUMN id SET DEFAULT nextval('user_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY user_message_pref ALTER COLUMN id SET DEFAULT nextval('user_message_pref_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY user_notification_pref ALTER COLUMN id SET DEFAULT nextval('user_notification_pref_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY user_setting ALTER COLUMN id SET DEFAULT nextval('user_setting_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY user_watching_project ALTER COLUMN id SET DEFAULT nextval('user_watching_project_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY view_comment ALTER COLUMN id SET DEFAULT nextval('view_comment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY view_time ALTER COLUMN id SET DEFAULT nextval('view_time_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY view_wiki_edit ALTER COLUMN id SET DEFAULT nextval('view_wiki_edit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY volunteer_application ALTER COLUMN id SET DEFAULT nextval('volunteer_application_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY volunteer_interest ALTER COLUMN id SET DEFAULT nextval('volunteer_interest_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_edit ALTER COLUMN id SET DEFAULT nextval('wiki_edit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_last_edit ALTER COLUMN id SET DEFAULT nextval('wiki_last_edit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_page ALTER COLUMN id SET DEFAULT nextval('wiki_page_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_page_comment ALTER COLUMN id SET DEFAULT nextval('wiki_page_comment_id_seq'::regclass);


--
-- Data for Name: account; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY account (id, balance) FROM stdin;
1	0
2	0
3	0
\.


--
-- Name: account_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('account_id_seq', 3, true);


--
-- Data for Name: build; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY build (id, boot_time, base, diff) FROM stdin;
1	2013-11-20 02:25:05.013214	8604247d8116396ec0214ba7677d2212990517d2	
2	2013-11-23 19:29:20.349772	8604247d8116396ec0214ba7677d2212990517d2	
3	2014-01-20 19:09:52.001477	4ccd10f42fd6d64ac4e2b3d70b28c9c6a3223b0c	diff --git a/Application.hs b/Application.hs\nindex 241f582..bf838a7 100644\n--- a/Application.hs\n+++ b/Application.hs\n@@ -135,8 +135,8 @@ doMigration = do\n \n     mapM_ ((\\ file -> liftIO (putStrLn ("running " ++ file ++ "...") >> T.readFile file)) >=> flip rawExecute []) $ L.map (("migrations/" <>) . snd) migration_files\n \n-    let last_migration = L.maximum $ 0 : L.map fst migration_files\n-    update $ flip set [ DatabaseVersionLastMigration =. val last_migration ]\n+    let new_last_migration = L.maximum $ 0 : L.map fst migration_files\n+    update $ flip set [ DatabaseVersionLastMigration =. val new_last_migration ]\n \n     migrations <- parseMigration' migrateAll\n \n@@ -145,8 +145,10 @@ doMigration = do\n     liftIO $ putStrLn $ "safe: " ++ show (L.length safe)\n     liftIO $ putStrLn $ "unsafe: " ++ show (L.length unsafe)\n \n+    liftIO $ putStrLn $ "new last_migration: " ++ show new_last_migration\n+\n     when (not $ L.null $ L.map snd safe) $ do\n-        liftIO $ T.writeFile ("migrations/migrate" <> show (last_migration + 1)) $ T.unlines $ L.map ((`snoc` ';') . snd) safe\n+        liftIO $ T.writeFile ("migrations/migrate" <> show (new_last_migration + 1)) $ T.unlines $ L.map ((`snoc` ';') . snd) safe\n         mapM_ (flip rawExecute [] . snd) migrations\n \n     when (not $ L.null $ L.map snd unsafe) $ do\n
4	2014-01-21 17:41:12.047799	ed014b5810941e61f82123f97be0c69600d99f0f	
5	2014-01-21 20:01:56.580066	ed014b5810941e61f82123f97be0c69600d99f0f	
6	2014-01-21 22:30:50.979424	d9c16e7c96ebb0c2e66e9f5e6d2c85abc7aa26dd	
7	2014-01-21 22:58:09.854227	d9c16e7c96ebb0c2e66e9f5e6d2c85abc7aa26dd	
8	2014-01-22 00:06:52.037741	f8d42e0d451310462b2840f2777126be8fd0b162	
9	2014-01-22 00:22:27.746213	1eea4b6d7e8a9a5b82e32a122f04d7611c9e29e3	
10	2014-01-24 06:17:39.744006	0bd171dd45776715e763bd42438f32f9494b4fa5	
11	2014-01-24 07:09:02.600052	0bd171dd45776715e763bd42438f32f9494b4fa5	diff --git a/templates/default-layout.cassius b/templates/default-layout.cassius\nindex adf4925..bb246ee 100644\n--- a/templates/default-layout.cassius\n+++ b/templates/default-layout.cassius\n@@ -19,7 +19,7 @@ img#logo_m\n \n p.navbar-text\n     font-size: 0.8em\n-    margin-top: 4px\n+    margin-top: 2px\n     margin-bottom: 0\n     padding: 0\n \ndiff --git a/templates/navbar.hamlet b/templates/navbar.hamlet\nindex 7cdc078..fe22941 100644\n--- a/templates/navbar.hamlet\n+++ b/templates/navbar.hamlet\n@@ -17,7 +17,7 @@\n             $maybe Entity user_id _ <- maybe_user\n                 $maybe (balance, pledged) <- money_info\n                     <p .navbar-text .text-center>\n-                        Pledged / Funds \n+                        Pledges / Funds \n                         <br>\n                         <a .navbar-link title="Current total monthly pledge" href="@{UserR user_id}">\n                             #{show pledged}\n
12	2014-01-24 07:10:14.401191	0bd171dd45776715e763bd42438f32f9494b4fa5	diff --git a/Handler/Wiki.hs b/Handler/Wiki.hs\nindex c2aeff5..c66e6e4 100644\n--- a/Handler/Wiki.hs\n+++ b/Handler/Wiki.hs\n@@ -35,7 +35,7 @@ getWikiR project_handle target = do\n \n     when (not can_edit) $ permissionDenied "you do not have permission to edit this page"\n \n-    defaultLayout $ renderWiki' project project_handle target can_edit True page\n+    defaultLayout $ renderWiki project_handle target can_edit True page\n \n renderWiki :: Text -> Text -> Bool -> Bool -> WikiPage -> Widget\n renderWiki project_handle target can_edit can_view_meta page = $(widgetFile "wiki")\ndiff --git a/templates/default-layout.cassius b/templates/default-layout.cassius\nindex adf4925..bb246ee 100644\n--- a/templates/default-layout.cassius\n+++ b/templates/default-layout.cassius\n@@ -19,7 +19,7 @@ img#logo_m\n \n p.navbar-text\n     font-size: 0.8em\n-    margin-top: 4px\n+    margin-top: 2px\n     margin-bottom: 0\n     padding: 0\n \ndiff --git a/templates/navbar.hamlet b/templates/navbar.hamlet\nindex 7cdc078..fe22941 100644\n--- a/templates/navbar.hamlet\n+++ b/templates/navbar.hamlet\n@@ -17,7 +17,7 @@\n             $maybe Entity user_id _ <- maybe_user\n                 $maybe (balance, pledged) <- money_info\n                     <p .navbar-text .text-center>\n-                        Pledged / Funds \n+                        Pledges / Funds \n                         <br>\n                         <a .navbar-link title="Current total monthly pledge" href="@{UserR user_id}">\n                             #{show pledged}\n
13	2014-01-24 07:14:01.500808	0bd171dd45776715e763bd42438f32f9494b4fa5	diff --git a/Handler/Wiki.hs b/Handler/Wiki.hs\nindex c2aeff5..a0d3d1e 100644\n--- a/Handler/Wiki.hs\n+++ b/Handler/Wiki.hs\n@@ -43,7 +43,7 @@ renderWiki project_handle target can_edit can_view_meta page = $(widgetFile "wik\n renderWiki' :: Project -> Text -> Text -> Bool -> Bool -> WikiPage -> Widget\n renderWiki' project project_handle target can_edit can_view_meta page = do\n     setTitle . toHtml $ projectName project `mappend` " Wiki - " `mappend` wikiPageTarget page `mappend` " | Snowdrift.coop"\n-    renderWiki project_handle target can_edit can_view_meta page\n+    renderWiki project_handle target can_edit True page\n \n \n getOldWikiPagesR :: Text -> Handler Html\ndiff --git a/templates/default-layout.cassius b/templates/default-layout.cassius\nindex adf4925..bb246ee 100644\n--- a/templates/default-layout.cassius\n+++ b/templates/default-layout.cassius\n@@ -19,7 +19,7 @@ img#logo_m\n \n p.navbar-text\n     font-size: 0.8em\n-    margin-top: 4px\n+    margin-top: 2px\n     margin-bottom: 0\n     padding: 0\n \ndiff --git a/templates/navbar.hamlet b/templates/navbar.hamlet\nindex 7cdc078..fe22941 100644\n--- a/templates/navbar.hamlet\n+++ b/templates/navbar.hamlet\n@@ -17,7 +17,7 @@\n             $maybe Entity user_id _ <- maybe_user\n                 $maybe (balance, pledged) <- money_info\n                     <p .navbar-text .text-center>\n-                        Pledged / Funds \n+                        Pledges / Funds \n                         <br>\n                         <a .navbar-link title="Current total monthly pledge" href="@{UserR user_id}">\n                             #{show pledged}\n
14	2014-01-24 07:23:58.983733	0bd171dd45776715e763bd42438f32f9494b4fa5	diff --git a/Handler/Wiki.hs b/Handler/Wiki.hs\nindex c2aeff5..a0d3d1e 100644\n--- a/Handler/Wiki.hs\n+++ b/Handler/Wiki.hs\n@@ -43,7 +43,7 @@ renderWiki project_handle target can_edit can_view_meta page = $(widgetFile "wik\n renderWiki' :: Project -> Text -> Text -> Bool -> Bool -> WikiPage -> Widget\n renderWiki' project project_handle target can_edit can_view_meta page = do\n     setTitle . toHtml $ projectName project `mappend` " Wiki - " `mappend` wikiPageTarget page `mappend` " | Snowdrift.coop"\n-    renderWiki project_handle target can_edit can_view_meta page\n+    renderWiki project_handle target can_edit True page\n \n \n getOldWikiPagesR :: Text -> Handler Html\ndiff --git a/templates/default-layout.cassius b/templates/default-layout.cassius\nindex adf4925..bb246ee 100644\n--- a/templates/default-layout.cassius\n+++ b/templates/default-layout.cassius\n@@ -19,7 +19,7 @@ img#logo_m\n \n p.navbar-text\n     font-size: 0.8em\n-    margin-top: 4px\n+    margin-top: 2px\n     margin-bottom: 0\n     padding: 0\n \ndiff --git a/templates/navbar.hamlet b/templates/navbar.hamlet\nindex 7cdc078..fe22941 100644\n--- a/templates/navbar.hamlet\n+++ b/templates/navbar.hamlet\n@@ -17,7 +17,7 @@\n             $maybe Entity user_id _ <- maybe_user\n                 $maybe (balance, pledged) <- money_info\n                     <p .navbar-text .text-center>\n-                        Pledged / Funds \n+                        Pledges / Funds \n                         <br>\n                         <a .navbar-link title="Current total monthly pledge" href="@{UserR user_id}">\n                             #{show pledged}\n
15	2014-01-24 07:53:46.525858	e05ef3d875769e1910908e96aed4ec096ecef498	diff --git a/Handler/Wiki.hs b/Handler/Wiki.hs\nindex c2aeff5..45b8ccb 100644\n--- a/Handler/Wiki.hs\n+++ b/Handler/Wiki.hs\n@@ -33,8 +33,6 @@ getWikiR project_handle target = do\n \n     let can_edit = isJust $ userEstablishedTs =<< entityVal <$> maybe_user\n \n-    when (not can_edit) $ permissionDenied "you do not have permission to edit this page"\n-\n     defaultLayout $ renderWiki' project project_handle target can_edit True page\n \n renderWiki :: Text -> Text -> Bool -> Bool -> WikiPage -> Widget\n
16	2014-01-24 18:52:45.121638	b857adcedd7be11cf2909b5d6cb536fb17d999c9	
17	2014-01-24 21:47:53.941683	b857adcedd7be11cf2909b5d6cb536fb17d999c9	
18	2014-01-24 23:28:23.255958	b857adcedd7be11cf2909b5d6cb536fb17d999c9	
19	2014-02-04 05:22:47.977252	e42ac19d7713acb15779eb289dc57697a265ffe3	
20	2014-03-02 05:31:59.002319	008e9bc87dbbab3764cfac6ac19bd3db630387d4	diff --git a/Import.hs b/Import.hs\nindex 09eb514..0c29391 100644\n--- a/Import.hs\n+++ b/Import.hs\n@@ -175,3 +175,4 @@ renderBootstrap3 aform fragment = do\n                 |]\n     return (res, widget)\n \n+\ndiff --git a/Settings.hs b/Settings.hs\nindex e303486..ad348e9 100644\n--- a/Settings.hs\n+++ b/Settings.hs\n@@ -57,8 +57,6 @@ widgetFileSettings = def\n         }\n     }\n \n--- The rest of this file contains settings which rarely need changing by a\n--- user.\n \n widgetFile :: String -> Q Exp\n widgetFile = (if development then widgetFileReload\ndiff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 16e0bd9..87f98b3 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -228,3 +228,5 @@ test-suite test\n                  , network\n                  , http-types\n                  , wai-test\n+                 , unix\n+                 , mtl\n
21	2014-05-27 18:35:44.545791	7939dab98e295e6d7cf43971b959009e32d8be1b	diff --git a/config/models b/config/models\nindex d9be8cb..5ac6377 100644\n--- a/config/models\n+++ b/config/models\n@@ -147,7 +147,7 @@ Message\n     from UserId Maybe\n     to UserId Maybe\n     content Markdown\n-    automated Bool default=False\n+    automated Bool default='f' \n \n \n WikiPage\ndiff --git a/devDB.sql b/devDB.sql\nindex 20c16a3..5e89af6 100644\n--- a/devDB.sql\n+++ b/devDB.sql\n@@ -3,7 +3,6 @@\n --\n \n SET statement_timeout = 0;\n-SET lock_timeout = 0;\n SET client_encoding = 'UTF8';\n SET standard_conforming_strings = on;\n SET check_function_bodies = false;\n@@ -151,8 +150,7 @@ CREATE TABLE comment (\n     "user" bigint NOT NULL,\n     text character varying NOT NULL,\n     depth bigint NOT NULL,\n-    discussion bigint NOT NULL,\n-    rethreaded bigint\n+    discussion bigint NOT NULL\n );\n \n \n@@ -193,22 +191,6 @@ ALTER SEQUENCE comment_ancestor_id_seq OWNED BY comment_ancestor.id;\n \n \n --\n--- Name: comment_closure; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: \n---\n-\n-CREATE TABLE comment_closure (\n-    id integer NOT NULL,\n-    ts timestamp without time zone NOT NULL,\n-    reason character varying NOT NULL,\n-    comment bigint NOT NULL,\n-    closed_by bigint NOT NULL,\n-    type character varying NOT NULL\n-);\n-\n-\n-ALTER TABLE public.comment_closure OWNER TO snowdrift_development;\n-\n---\n -- Name: comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development\n --\n \n@@ -235,9 +217,14 @@ ALTER SEQUENCE comment_id_seq OWNED BY comment.id;\n \n CREATE TABLE comment_rethread (\n     id integer NOT NULL,\n-    rethread bigint NOT NULL,\n-    old_comment bigint NOT NULL,\n-    new_comment bigint NOT NULL\n+    ts timestamp without time zone NOT NULL,\n+    moderator bigint NOT NULL,\n+    old_parent bigint,\n+    old_discussion bigint NOT NULL,\n+    new_parent bigint,\n+    new_discussion bigint NOT NULL,\n+    comment bigint NOT NULL,\n+    reason character varying NOT NULL\n );\n \n \n@@ -296,28 +283,7 @@ ALTER TABLE public.comment_retraction_id_seq OWNER TO snowdrift_development;\n -- Name: comment_retraction_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER SEQUENCE comment_retraction_id_seq OWNED BY comment_closure.id;\n-\n-\n---\n--- Name: comment_retraction_id_seq1; Type: SEQUENCE; Schema: public; Owner: snowdrift_development\n---\n-\n-CREATE SEQUENCE comment_retraction_id_seq1\n-    START WITH 1\n-    INCREMENT BY 1\n-    NO MINVALUE\n-    NO MAXVALUE\n-    CACHE 1;\n-\n-\n-ALTER TABLE public.comment_retraction_id_seq1 OWNER TO snowdrift_development;\n-\n---\n--- Name: comment_retraction_id_seq1; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER SEQUENCE comment_retraction_id_seq1 OWNED BY comment_retraction.id;\n+ALTER SEQUENCE comment_retraction_id_seq OWNED BY comment_retraction.id;\n \n \n --\n@@ -678,8 +644,7 @@ CREATE TABLE message (\n     created_ts timestamp without time zone NOT NULL,\n     "from" bigint,\n     "to" bigint,\n-    content character varying NOT NULL,\n-    automated boolean DEFAULT false NOT NULL\n+    content character varying NOT NULL\n );\n \n \n@@ -1028,42 +993,6 @@ ALTER SEQUENCE project_user_role_id_seq OWNED BY project_user_role.id;\n \n \n --\n--- Name: rethread; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: \n---\n-\n-CREATE TABLE rethread (\n-    id integer NOT NULL,\n-    ts timestamp without time zone NOT NULL,\n-    moderator bigint NOT NULL,\n-    old_comment bigint NOT NULL,\n-    reason character varying NOT NULL\n-);\n-\n-\n-ALTER TABLE public.rethread OWNER TO snowdrift_development;\n-\n---\n--- Name: rethread_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development\n---\n-\n-CREATE SEQUENCE rethread_id_seq\n-    START WITH 1\n-    INCREMENT BY 1\n-    NO MINVALUE\n-    NO MAXVALUE\n-    CACHE 1;\n-\n-\n-ALTER TABLE public.rethread_id_seq OWNER TO snowdrift_development;\n-\n---\n--- Name: rethread_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER SEQUENCE rethread_id_seq OWNED BY rethread.id;\n-\n-\n---\n -- Name: role_event; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: \n --\n \n@@ -1612,13 +1541,6 @@ ALTER TABLE ONLY comment_ancestor ALTER COLUMN id SET DEFAULT nextval('comment_a\n -- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY comment_closure ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq'::regclass);\n-\n-\n---\n--- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n---\n-\n ALTER TABLE ONLY comment_rethread ALTER COLUMN id SET DEFAULT nextval('comment_rethread_id_seq'::regclass);\n \n \n@@ -1626,7 +1548,7 @@ ALTER TABLE ONLY comment_rethread ALTER COLUMN id SET DEFAULT nextval('comment_r\n -- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY comment_retraction ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq1'::regclass);\n+ALTER TABLE ONLY comment_retraction ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq'::regclass);\n \n \n --\n@@ -1773,13 +1695,6 @@ ALTER TABLE ONLY project_user_role ALTER COLUMN id SET DEFAULT nextval('project_\n -- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY rethread ALTER COLUMN id SET DEFAULT nextval('rethread_id_seq'::regclass);\n-\n-\n---\n--- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n---\n-\n ALTER TABLE ONLY role_event ALTER COLUMN id SET DEFAULT nextval('role_event_id_seq'::regclass);\n \n \n@@ -1917,7 +1832,6 @@ COPY build (id, boot_time, base, diff) FROM stdin;\n 18\t2014-01-24 23:28:23.255958\tb857adcedd7be11cf2909b5d6cb536fb17d999c9\t\n 19\t2014-02-04 05:22:47.977252\te42ac19d7713acb15779eb289dc57697a265ffe3\t\n 20\t2014-03-02 05:31:59.002319\t008e9bc87dbbab3764cfac6ac19bd3db630387d4\tdiff --git a/Import.hs b/Import.hs\\nindex 09eb514..0c29391 100644\\n--- a/Import.hs\\n+++ b/Import.hs\\n@@ -175,3 +175,4 @@ renderBootstrap3 aform fragment = do\\n                 |]\\n     return (res, widget)\\n \\n+\\ndiff --git a/Settings.hs b/Settings.hs\\nindex e303486..ad348e9 100644\\n--- a/Settings.hs\\n+++ b/Settings.hs\\n@@ -57,8 +57,6 @@ widgetFileSettings = def\\n         }\\n     }\\n \\n--- The rest of this file contains settings which rarely need changing by a\\n--- user.\\n \\n widgetFile :: String -> Q Exp\\n widgetFile = (if development then widgetFileReload\\ndiff --git a/Snowdrift.cabal b/Snowdrift.cabal\\nindex 16e0bd9..87f98b3 100644\\n--- a/Snowdrift.cabal\\n+++ b/Snowdrift.cabal\\n@@ -228,3 +228,5 @@ test-suite test\\n                  , network\\n                  , http-types\\n                  , wai-test\\n+                 , unix\\n+                 , mtl\\n\n-21\t2014-05-27 16:23:29.728077\te81380ff27267098517bd582269afd7780a4a517\tdiff --git a/README.md b/README.md\\nindex 25ea3e8..2bbd67e 100644\\n--- a/README.md\\n+++ b/README.md\\n@@ -189,7 +189,9 @@ Then add user to database:\\n \\n     postgres=# grant all privileges on database snowdrift_development to snowdrift_development;\\n \\n-Leave postgres (with ctrl-D), then edit config/postgresql.yml and update the password to match the one you entered.\\n+Leave postgres (with ctrl-D).\\n+\\n+Edit config/postgresql.yml and update the password to match the one you entered.\\n \\n Import development database:\\n \\n@@ -355,7 +357,7 @@ Go to the postgres=# prompt:\\n \\n     sudo -u postgres psql\\n \\n-Unmark the template:\\n+Unmark the template (don't include the postgres=# prompt part):\\n \\n     postgres=# update pg_database set datistemplate=false where datname='snowdrift_test_template';\\n           \\n\n \\.\n \n \n@@ -1925,18 +1839,18 @@ COPY build (id, boot_time, base, diff) FROM stdin;\n -- Name: build_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development\n --\n \n-SELECT pg_catalog.setval('build_id_seq', 21, true);\n+SELECT pg_catalog.setval('build_id_seq', 20, true);\n \n \n --\n -- Data for Name: comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n-COPY comment (id, created_ts, moderated_ts, moderated_by, parent, "user", text, depth, discussion, rethreaded) FROM stdin;\n-1\t2014-01-21 18:11:03.914397\t2014-01-21 18:12:36.696658\t1\t\\N\t1\tThis is a comment.\t0\t2\t\\N\n-2\t2014-01-21 18:13:00.273315\t2014-01-21 18:13:10.464805\t1\t1\t1\tReplies are threaded.\t1\t2\t\\N\n-3\t2014-01-21 18:13:57.732222\t\\N\t\\N\t\\N\t1\tWhen a comment is posted by an unestablished user, it is marked for moderation and only shown to moderators.\t0\t2\t\\N\n-4\t2014-01-21 18:15:30.945499\t2014-01-21 18:15:37.484472\t1\t\\N\t1\tadding a line starting with "ticket:" such as\\n\\nticket: this is a ticket\\n\\nmakes the post show up at /t where all the tickets are listed\t0\t2\t\\N\n+COPY comment (id, created_ts, moderated_ts, moderated_by, parent, "user", text, depth, discussion) FROM stdin;\n+1\t2014-01-21 18:11:03.914397\t2014-01-21 18:12:36.696658\t1\t\\N\t1\tThis is a comment.\t0\t2\n+2\t2014-01-21 18:13:00.273315\t2014-01-21 18:13:10.464805\t1\t1\t1\tReplies are threaded.\t1\t2\n+3\t2014-01-21 18:13:57.732222\t\\N\t\\N\t\\N\t1\tWhen a comment is posted by an unestablished user, it is marked for moderation and only shown to moderators.\t0\t2\n+4\t2014-01-21 18:15:30.945499\t2014-01-21 18:15:37.484472\t1\t\\N\t1\tadding a line starting with "ticket:" such as\\n\\nticket: this is a ticket\\n\\nmakes the post show up at /t where all the tickets are listed\t0\t2\n \\.\n \n \n@@ -1957,14 +1871,6 @@ SELECT pg_catalog.setval('comment_ancestor_id_seq', 1, true);\n \n \n --\n--- Data for Name: comment_closure; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n---\n-\n-COPY comment_closure (id, ts, reason, comment, closed_by, type) FROM stdin;\n-\\.\n-\n-\n---\n -- Name: comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development\n --\n \n@@ -1975,7 +1881,7 @@ SELECT pg_catalog.setval('comment_id_seq', 4, true);\n -- Data for Name: comment_rethread; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n-COPY comment_rethread (id, rethread, old_comment, new_comment) FROM stdin;\n+COPY comment_rethread (id, ts, moderator, old_parent, old_discussion, new_parent, new_discussion, comment, reason) FROM stdin;\n \\.\n \n \n@@ -2002,13 +1908,6 @@ SELECT pg_catalog.setval('comment_retraction_id_seq', 1, false);\n \n \n --\n--- Name: comment_retraction_id_seq1; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development\n---\n-\n-SELECT pg_catalog.setval('comment_retraction_id_seq1', 1, false);\n-\n-\n---\n -- Data for Name: comment_tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n@@ -2043,7 +1942,7 @@ SELECT pg_catalog.setval('committee_user_id_seq', 1, false);\n --\n \n COPY database_version (id, last_migration) FROM stdin;\n-1\t7\n+1\t5\n \\.\n \n \n@@ -2169,8 +2068,8 @@ SELECT pg_catalog.setval('manual_establishment_id_seq', 1, false);\n -- Data for Name: message; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n-COPY message (id, project, created_ts, "from", "to", content, automated) FROM stdin;\n-1\t1\t2014-01-21 22:31:51.496246\t1\t\\N\tWelcome!\tf\n+COPY message (id, project, created_ts, "from", "to", content) FROM stdin;\n+1\t1\t2014-01-21 22:31:51.496246\t1\t\\N\tWelcome!\n \\.\n \n \n@@ -2324,21 +2223,6 @@ SELECT pg_catalog.setval('project_user_role_id_seq', 4, true);\n \n \n --\n--- Data for Name: rethread; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n---\n-\n-COPY rethread (id, ts, moderator, old_comment, reason) FROM stdin;\n-\\.\n-\n-\n---\n--- Name: rethread_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development\n---\n-\n-SELECT pg_catalog.setval('rethread_id_seq', 1, false);\n-\n-\n---\n -- Data for Name: role_event; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n@@ -2616,16 +2500,8 @@ ALTER TABLE ONLY comment_rethread\n -- Name: comment_retraction_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: \n --\n \n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_retraction_pkey PRIMARY KEY (id);\n-\n-\n---\n--- Name: comment_retraction_pkey1; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: \n---\n-\n ALTER TABLE ONLY comment_retraction\n-    ADD CONSTRAINT comment_retraction_pkey1 PRIMARY KEY (id);\n+    ADD CONSTRAINT comment_retraction_pkey PRIMARY KEY (id);\n \n \n --\n@@ -2789,14 +2665,6 @@ ALTER TABLE ONLY project_user_role\n \n \n --\n--- Name: rethread_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: \n---\n-\n-ALTER TABLE ONLY rethread\n-    ADD CONSTRAINT rethread_pkey PRIMARY KEY (id);\n-\n-\n---\n -- Name: role_event_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: \n --\n \n@@ -3115,30 +2983,6 @@ ALTER TABLE ONLY comment_ancestor\n \n \n --\n--- Name: comment_closure_closed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_closure_closed_by_fkey FOREIGN KEY (closed_by) REFERENCES "user"(id);\n-\n-\n---\n--- Name: comment_closure_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_closure_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);\n-\n-\n---\n--- Name: comment_closure_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_closure_user_fkey FOREIGN KEY (closed_by) REFERENCES "user"(id);\n-\n-\n---\n -- Name: comment_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n@@ -3171,43 +3015,51 @@ ALTER TABLE ONLY comment\n \n \n --\n--- Name: comment_rethread_new_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n ALTER TABLE ONLY comment_rethread\n-    ADD CONSTRAINT comment_rethread_new_comment_fkey FOREIGN KEY (new_comment) REFERENCES comment(id);\n+    ADD CONSTRAINT comment_rethread_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);\n \n \n --\n--- Name: comment_rethread_old_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_moderator_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n ALTER TABLE ONLY comment_rethread\n-    ADD CONSTRAINT comment_rethread_old_comment_fkey FOREIGN KEY (old_comment) REFERENCES comment(id);\n+    ADD CONSTRAINT comment_rethread_moderator_fkey FOREIGN KEY (moderator) REFERENCES "user"(id);\n \n \n --\n--- Name: comment_rethread_rethread_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_new_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n ALTER TABLE ONLY comment_rethread\n-    ADD CONSTRAINT comment_rethread_rethread_fkey FOREIGN KEY (rethread) REFERENCES rethread(id);\n+    ADD CONSTRAINT comment_rethread_new_discussion_fkey FOREIGN KEY (new_discussion) REFERENCES discussion(id);\n \n \n --\n--- Name: comment_rethreaded_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_new_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY comment\n-    ADD CONSTRAINT comment_rethreaded_fkey FOREIGN KEY (rethreaded) REFERENCES rethread(id);\n+ALTER TABLE ONLY comment_rethread\n+    ADD CONSTRAINT comment_rethread_new_parent_fkey FOREIGN KEY (new_parent) REFERENCES comment(id);\n \n \n --\n--- Name: comment_retraction_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_old_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_retraction_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);\n+ALTER TABLE ONLY comment_rethread\n+    ADD CONSTRAINT comment_rethread_old_discussion_fkey FOREIGN KEY (old_discussion) REFERENCES discussion(id);\n+\n+\n+--\n+-- Name: comment_rethread_old_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+--\n+\n+ALTER TABLE ONLY comment_rethread\n+    ADD CONSTRAINT comment_rethread_old_parent_fkey FOREIGN KEY (old_parent) REFERENCES comment(id);\n \n \n --\n@@ -3483,22 +3335,6 @@ ALTER TABLE ONLY project_user_role\n \n \n --\n--- Name: rethread_moderator_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY rethread\n-    ADD CONSTRAINT rethread_moderator_fkey FOREIGN KEY (moderator) REFERENCES "user"(id);\n-\n-\n---\n--- Name: rethread_old_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY rethread\n-    ADD CONSTRAINT rethread_old_comment_fkey FOREIGN KEY (old_comment) REFERENCES comment(id);\n-\n-\n---\n -- Name: role_event_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \ndiff --git a/migrations/migrate8 b/migrations/migrate8\ndeleted file mode 100644\nindex d30aabf..0000000\n--- a/migrations/migrate8\n+++ /dev/null\n@@ -1 +0,0 @@\n-ALTER TABLE "message" ADD COLUMN "automated" BOOLEAN NOT NULL DEFAULT False;\n
22	2014-05-27 18:36:21.415917	7939dab98e295e6d7cf43971b959009e32d8be1b	diff --git a/config/models b/config/models\nindex d9be8cb..5ac6377 100644\n--- a/config/models\n+++ b/config/models\n@@ -147,7 +147,7 @@ Message\n     from UserId Maybe\n     to UserId Maybe\n     content Markdown\n-    automated Bool default=False\n+    automated Bool default='f' \n \n \n WikiPage\ndiff --git a/devDB.sql b/devDB.sql\nindex 20c16a3..5e89af6 100644\n--- a/devDB.sql\n+++ b/devDB.sql\n@@ -3,7 +3,6 @@\n --\n \n SET statement_timeout = 0;\n-SET lock_timeout = 0;\n SET client_encoding = 'UTF8';\n SET standard_conforming_strings = on;\n SET check_function_bodies = false;\n@@ -151,8 +150,7 @@ CREATE TABLE comment (\n     "user" bigint NOT NULL,\n     text character varying NOT NULL,\n     depth bigint NOT NULL,\n-    discussion bigint NOT NULL,\n-    rethreaded bigint\n+    discussion bigint NOT NULL\n );\n \n \n@@ -193,22 +191,6 @@ ALTER SEQUENCE comment_ancestor_id_seq OWNED BY comment_ancestor.id;\n \n \n --\n--- Name: comment_closure; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: \n---\n-\n-CREATE TABLE comment_closure (\n-    id integer NOT NULL,\n-    ts timestamp without time zone NOT NULL,\n-    reason character varying NOT NULL,\n-    comment bigint NOT NULL,\n-    closed_by bigint NOT NULL,\n-    type character varying NOT NULL\n-);\n-\n-\n-ALTER TABLE public.comment_closure OWNER TO snowdrift_development;\n-\n---\n -- Name: comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development\n --\n \n@@ -235,9 +217,14 @@ ALTER SEQUENCE comment_id_seq OWNED BY comment.id;\n \n CREATE TABLE comment_rethread (\n     id integer NOT NULL,\n-    rethread bigint NOT NULL,\n-    old_comment bigint NOT NULL,\n-    new_comment bigint NOT NULL\n+    ts timestamp without time zone NOT NULL,\n+    moderator bigint NOT NULL,\n+    old_parent bigint,\n+    old_discussion bigint NOT NULL,\n+    new_parent bigint,\n+    new_discussion bigint NOT NULL,\n+    comment bigint NOT NULL,\n+    reason character varying NOT NULL\n );\n \n \n@@ -296,28 +283,7 @@ ALTER TABLE public.comment_retraction_id_seq OWNER TO snowdrift_development;\n -- Name: comment_retraction_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER SEQUENCE comment_retraction_id_seq OWNED BY comment_closure.id;\n-\n-\n---\n--- Name: comment_retraction_id_seq1; Type: SEQUENCE; Schema: public; Owner: snowdrift_development\n---\n-\n-CREATE SEQUENCE comment_retraction_id_seq1\n-    START WITH 1\n-    INCREMENT BY 1\n-    NO MINVALUE\n-    NO MAXVALUE\n-    CACHE 1;\n-\n-\n-ALTER TABLE public.comment_retraction_id_seq1 OWNER TO snowdrift_development;\n-\n---\n--- Name: comment_retraction_id_seq1; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER SEQUENCE comment_retraction_id_seq1 OWNED BY comment_retraction.id;\n+ALTER SEQUENCE comment_retraction_id_seq OWNED BY comment_retraction.id;\n \n \n --\n@@ -678,8 +644,7 @@ CREATE TABLE message (\n     created_ts timestamp without time zone NOT NULL,\n     "from" bigint,\n     "to" bigint,\n-    content character varying NOT NULL,\n-    automated boolean DEFAULT false NOT NULL\n+    content character varying NOT NULL\n );\n \n \n@@ -1028,42 +993,6 @@ ALTER SEQUENCE project_user_role_id_seq OWNED BY project_user_role.id;\n \n \n --\n--- Name: rethread; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: \n---\n-\n-CREATE TABLE rethread (\n-    id integer NOT NULL,\n-    ts timestamp without time zone NOT NULL,\n-    moderator bigint NOT NULL,\n-    old_comment bigint NOT NULL,\n-    reason character varying NOT NULL\n-);\n-\n-\n-ALTER TABLE public.rethread OWNER TO snowdrift_development;\n-\n---\n--- Name: rethread_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development\n---\n-\n-CREATE SEQUENCE rethread_id_seq\n-    START WITH 1\n-    INCREMENT BY 1\n-    NO MINVALUE\n-    NO MAXVALUE\n-    CACHE 1;\n-\n-\n-ALTER TABLE public.rethread_id_seq OWNER TO snowdrift_development;\n-\n---\n--- Name: rethread_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER SEQUENCE rethread_id_seq OWNED BY rethread.id;\n-\n-\n---\n -- Name: role_event; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: \n --\n \n@@ -1612,13 +1541,6 @@ ALTER TABLE ONLY comment_ancestor ALTER COLUMN id SET DEFAULT nextval('comment_a\n -- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY comment_closure ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq'::regclass);\n-\n-\n---\n--- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n---\n-\n ALTER TABLE ONLY comment_rethread ALTER COLUMN id SET DEFAULT nextval('comment_rethread_id_seq'::regclass);\n \n \n@@ -1626,7 +1548,7 @@ ALTER TABLE ONLY comment_rethread ALTER COLUMN id SET DEFAULT nextval('comment_r\n -- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY comment_retraction ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq1'::regclass);\n+ALTER TABLE ONLY comment_retraction ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq'::regclass);\n \n \n --\n@@ -1773,13 +1695,6 @@ ALTER TABLE ONLY project_user_role ALTER COLUMN id SET DEFAULT nextval('project_\n -- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY rethread ALTER COLUMN id SET DEFAULT nextval('rethread_id_seq'::regclass);\n-\n-\n---\n--- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n---\n-\n ALTER TABLE ONLY role_event ALTER COLUMN id SET DEFAULT nextval('role_event_id_seq'::regclass);\n \n \n@@ -1917,7 +1832,6 @@ COPY build (id, boot_time, base, diff) FROM stdin;\n 18\t2014-01-24 23:28:23.255958\tb857adcedd7be11cf2909b5d6cb536fb17d999c9\t\n 19\t2014-02-04 05:22:47.977252\te42ac19d7713acb15779eb289dc57697a265ffe3\t\n 20\t2014-03-02 05:31:59.002319\t008e9bc87dbbab3764cfac6ac19bd3db630387d4\tdiff --git a/Import.hs b/Import.hs\\nindex 09eb514..0c29391 100644\\n--- a/Import.hs\\n+++ b/Import.hs\\n@@ -175,3 +175,4 @@ renderBootstrap3 aform fragment = do\\n                 |]\\n     return (res, widget)\\n \\n+\\ndiff --git a/Settings.hs b/Settings.hs\\nindex e303486..ad348e9 100644\\n--- a/Settings.hs\\n+++ b/Settings.hs\\n@@ -57,8 +57,6 @@ widgetFileSettings = def\\n         }\\n     }\\n \\n--- The rest of this file contains settings which rarely need changing by a\\n--- user.\\n \\n widgetFile :: String -> Q Exp\\n widgetFile = (if development then widgetFileReload\\ndiff --git a/Snowdrift.cabal b/Snowdrift.cabal\\nindex 16e0bd9..87f98b3 100644\\n--- a/Snowdrift.cabal\\n+++ b/Snowdrift.cabal\\n@@ -228,3 +228,5 @@ test-suite test\\n                  , network\\n                  , http-types\\n                  , wai-test\\n+                 , unix\\n+                 , mtl\\n\n-21\t2014-05-27 16:23:29.728077\te81380ff27267098517bd582269afd7780a4a517\tdiff --git a/README.md b/README.md\\nindex 25ea3e8..2bbd67e 100644\\n--- a/README.md\\n+++ b/README.md\\n@@ -189,7 +189,9 @@ Then add user to database:\\n \\n     postgres=# grant all privileges on database snowdrift_development to snowdrift_development;\\n \\n-Leave postgres (with ctrl-D), then edit config/postgresql.yml and update the password to match the one you entered.\\n+Leave postgres (with ctrl-D).\\n+\\n+Edit config/postgresql.yml and update the password to match the one you entered.\\n \\n Import development database:\\n \\n@@ -355,7 +357,7 @@ Go to the postgres=# prompt:\\n \\n     sudo -u postgres psql\\n \\n-Unmark the template:\\n+Unmark the template (don't include the postgres=# prompt part):\\n \\n     postgres=# update pg_database set datistemplate=false where datname='snowdrift_test_template';\\n           \\n\n \\.\n \n \n@@ -1925,18 +1839,18 @@ COPY build (id, boot_time, base, diff) FROM stdin;\n -- Name: build_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development\n --\n \n-SELECT pg_catalog.setval('build_id_seq', 21, true);\n+SELECT pg_catalog.setval('build_id_seq', 20, true);\n \n \n --\n -- Data for Name: comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n-COPY comment (id, created_ts, moderated_ts, moderated_by, parent, "user", text, depth, discussion, rethreaded) FROM stdin;\n-1\t2014-01-21 18:11:03.914397\t2014-01-21 18:12:36.696658\t1\t\\N\t1\tThis is a comment.\t0\t2\t\\N\n-2\t2014-01-21 18:13:00.273315\t2014-01-21 18:13:10.464805\t1\t1\t1\tReplies are threaded.\t1\t2\t\\N\n-3\t2014-01-21 18:13:57.732222\t\\N\t\\N\t\\N\t1\tWhen a comment is posted by an unestablished user, it is marked for moderation and only shown to moderators.\t0\t2\t\\N\n-4\t2014-01-21 18:15:30.945499\t2014-01-21 18:15:37.484472\t1\t\\N\t1\tadding a line starting with "ticket:" such as\\n\\nticket: this is a ticket\\n\\nmakes the post show up at /t where all the tickets are listed\t0\t2\t\\N\n+COPY comment (id, created_ts, moderated_ts, moderated_by, parent, "user", text, depth, discussion) FROM stdin;\n+1\t2014-01-21 18:11:03.914397\t2014-01-21 18:12:36.696658\t1\t\\N\t1\tThis is a comment.\t0\t2\n+2\t2014-01-21 18:13:00.273315\t2014-01-21 18:13:10.464805\t1\t1\t1\tReplies are threaded.\t1\t2\n+3\t2014-01-21 18:13:57.732222\t\\N\t\\N\t\\N\t1\tWhen a comment is posted by an unestablished user, it is marked for moderation and only shown to moderators.\t0\t2\n+4\t2014-01-21 18:15:30.945499\t2014-01-21 18:15:37.484472\t1\t\\N\t1\tadding a line starting with "ticket:" such as\\n\\nticket: this is a ticket\\n\\nmakes the post show up at /t where all the tickets are listed\t0\t2\n \\.\n \n \n@@ -1957,14 +1871,6 @@ SELECT pg_catalog.setval('comment_ancestor_id_seq', 1, true);\n \n \n --\n--- Data for Name: comment_closure; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n---\n-\n-COPY comment_closure (id, ts, reason, comment, closed_by, type) FROM stdin;\n-\\.\n-\n-\n---\n -- Name: comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development\n --\n \n@@ -1975,7 +1881,7 @@ SELECT pg_catalog.setval('comment_id_seq', 4, true);\n -- Data for Name: comment_rethread; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n-COPY comment_rethread (id, rethread, old_comment, new_comment) FROM stdin;\n+COPY comment_rethread (id, ts, moderator, old_parent, old_discussion, new_parent, new_discussion, comment, reason) FROM stdin;\n \\.\n \n \n@@ -2002,13 +1908,6 @@ SELECT pg_catalog.setval('comment_retraction_id_seq', 1, false);\n \n \n --\n--- Name: comment_retraction_id_seq1; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development\n---\n-\n-SELECT pg_catalog.setval('comment_retraction_id_seq1', 1, false);\n-\n-\n---\n -- Data for Name: comment_tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n@@ -2043,7 +1942,7 @@ SELECT pg_catalog.setval('committee_user_id_seq', 1, false);\n --\n \n COPY database_version (id, last_migration) FROM stdin;\n-1\t7\n+1\t5\n \\.\n \n \n@@ -2169,8 +2068,8 @@ SELECT pg_catalog.setval('manual_establishment_id_seq', 1, false);\n -- Data for Name: message; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n-COPY message (id, project, created_ts, "from", "to", content, automated) FROM stdin;\n-1\t1\t2014-01-21 22:31:51.496246\t1\t\\N\tWelcome!\tf\n+COPY message (id, project, created_ts, "from", "to", content) FROM stdin;\n+1\t1\t2014-01-21 22:31:51.496246\t1\t\\N\tWelcome!\n \\.\n \n \n@@ -2324,21 +2223,6 @@ SELECT pg_catalog.setval('project_user_role_id_seq', 4, true);\n \n \n --\n--- Data for Name: rethread; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n---\n-\n-COPY rethread (id, ts, moderator, old_comment, reason) FROM stdin;\n-\\.\n-\n-\n---\n--- Name: rethread_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development\n---\n-\n-SELECT pg_catalog.setval('rethread_id_seq', 1, false);\n-\n-\n---\n -- Data for Name: role_event; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n@@ -2616,16 +2500,8 @@ ALTER TABLE ONLY comment_rethread\n -- Name: comment_retraction_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: \n --\n \n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_retraction_pkey PRIMARY KEY (id);\n-\n-\n---\n--- Name: comment_retraction_pkey1; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: \n---\n-\n ALTER TABLE ONLY comment_retraction\n-    ADD CONSTRAINT comment_retraction_pkey1 PRIMARY KEY (id);\n+    ADD CONSTRAINT comment_retraction_pkey PRIMARY KEY (id);\n \n \n --\n@@ -2789,14 +2665,6 @@ ALTER TABLE ONLY project_user_role\n \n \n --\n--- Name: rethread_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: \n---\n-\n-ALTER TABLE ONLY rethread\n-    ADD CONSTRAINT rethread_pkey PRIMARY KEY (id);\n-\n-\n---\n -- Name: role_event_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: \n --\n \n@@ -3115,30 +2983,6 @@ ALTER TABLE ONLY comment_ancestor\n \n \n --\n--- Name: comment_closure_closed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_closure_closed_by_fkey FOREIGN KEY (closed_by) REFERENCES "user"(id);\n-\n-\n---\n--- Name: comment_closure_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_closure_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);\n-\n-\n---\n--- Name: comment_closure_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_closure_user_fkey FOREIGN KEY (closed_by) REFERENCES "user"(id);\n-\n-\n---\n -- Name: comment_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n@@ -3171,43 +3015,51 @@ ALTER TABLE ONLY comment\n \n \n --\n--- Name: comment_rethread_new_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n ALTER TABLE ONLY comment_rethread\n-    ADD CONSTRAINT comment_rethread_new_comment_fkey FOREIGN KEY (new_comment) REFERENCES comment(id);\n+    ADD CONSTRAINT comment_rethread_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);\n \n \n --\n--- Name: comment_rethread_old_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_moderator_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n ALTER TABLE ONLY comment_rethread\n-    ADD CONSTRAINT comment_rethread_old_comment_fkey FOREIGN KEY (old_comment) REFERENCES comment(id);\n+    ADD CONSTRAINT comment_rethread_moderator_fkey FOREIGN KEY (moderator) REFERENCES "user"(id);\n \n \n --\n--- Name: comment_rethread_rethread_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_new_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n ALTER TABLE ONLY comment_rethread\n-    ADD CONSTRAINT comment_rethread_rethread_fkey FOREIGN KEY (rethread) REFERENCES rethread(id);\n+    ADD CONSTRAINT comment_rethread_new_discussion_fkey FOREIGN KEY (new_discussion) REFERENCES discussion(id);\n \n \n --\n--- Name: comment_rethreaded_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_new_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY comment\n-    ADD CONSTRAINT comment_rethreaded_fkey FOREIGN KEY (rethreaded) REFERENCES rethread(id);\n+ALTER TABLE ONLY comment_rethread\n+    ADD CONSTRAINT comment_rethread_new_parent_fkey FOREIGN KEY (new_parent) REFERENCES comment(id);\n \n \n --\n--- Name: comment_retraction_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_old_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_retraction_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);\n+ALTER TABLE ONLY comment_rethread\n+    ADD CONSTRAINT comment_rethread_old_discussion_fkey FOREIGN KEY (old_discussion) REFERENCES discussion(id);\n+\n+\n+--\n+-- Name: comment_rethread_old_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+--\n+\n+ALTER TABLE ONLY comment_rethread\n+    ADD CONSTRAINT comment_rethread_old_parent_fkey FOREIGN KEY (old_parent) REFERENCES comment(id);\n \n \n --\n@@ -3483,22 +3335,6 @@ ALTER TABLE ONLY project_user_role\n \n \n --\n--- Name: rethread_moderator_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY rethread\n-    ADD CONSTRAINT rethread_moderator_fkey FOREIGN KEY (moderator) REFERENCES "user"(id);\n-\n-\n---\n--- Name: rethread_old_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY rethread\n-    ADD CONSTRAINT rethread_old_comment_fkey FOREIGN KEY (old_comment) REFERENCES comment(id);\n-\n-\n---\n -- Name: role_event_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \ndiff --git a/migrations/migrate8 b/migrations/migrate8\ndeleted file mode 100644\nindex d30aabf..0000000\n--- a/migrations/migrate8\n+++ /dev/null\n@@ -1 +0,0 @@\n-ALTER TABLE "message" ADD COLUMN "automated" BOOLEAN NOT NULL DEFAULT False;\n
23	2014-05-27 18:36:39.687391	7939dab98e295e6d7cf43971b959009e32d8be1b	diff --git a/config/models b/config/models\nindex d9be8cb..5ac6377 100644\n--- a/config/models\n+++ b/config/models\n@@ -147,7 +147,7 @@ Message\n     from UserId Maybe\n     to UserId Maybe\n     content Markdown\n-    automated Bool default=False\n+    automated Bool default='f' \n \n \n WikiPage\ndiff --git a/devDB.sql b/devDB.sql\nindex 20c16a3..5e89af6 100644\n--- a/devDB.sql\n+++ b/devDB.sql\n@@ -3,7 +3,6 @@\n --\n \n SET statement_timeout = 0;\n-SET lock_timeout = 0;\n SET client_encoding = 'UTF8';\n SET standard_conforming_strings = on;\n SET check_function_bodies = false;\n@@ -151,8 +150,7 @@ CREATE TABLE comment (\n     "user" bigint NOT NULL,\n     text character varying NOT NULL,\n     depth bigint NOT NULL,\n-    discussion bigint NOT NULL,\n-    rethreaded bigint\n+    discussion bigint NOT NULL\n );\n \n \n@@ -193,22 +191,6 @@ ALTER SEQUENCE comment_ancestor_id_seq OWNED BY comment_ancestor.id;\n \n \n --\n--- Name: comment_closure; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: \n---\n-\n-CREATE TABLE comment_closure (\n-    id integer NOT NULL,\n-    ts timestamp without time zone NOT NULL,\n-    reason character varying NOT NULL,\n-    comment bigint NOT NULL,\n-    closed_by bigint NOT NULL,\n-    type character varying NOT NULL\n-);\n-\n-\n-ALTER TABLE public.comment_closure OWNER TO snowdrift_development;\n-\n---\n -- Name: comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development\n --\n \n@@ -235,9 +217,14 @@ ALTER SEQUENCE comment_id_seq OWNED BY comment.id;\n \n CREATE TABLE comment_rethread (\n     id integer NOT NULL,\n-    rethread bigint NOT NULL,\n-    old_comment bigint NOT NULL,\n-    new_comment bigint NOT NULL\n+    ts timestamp without time zone NOT NULL,\n+    moderator bigint NOT NULL,\n+    old_parent bigint,\n+    old_discussion bigint NOT NULL,\n+    new_parent bigint,\n+    new_discussion bigint NOT NULL,\n+    comment bigint NOT NULL,\n+    reason character varying NOT NULL\n );\n \n \n@@ -296,28 +283,7 @@ ALTER TABLE public.comment_retraction_id_seq OWNER TO snowdrift_development;\n -- Name: comment_retraction_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER SEQUENCE comment_retraction_id_seq OWNED BY comment_closure.id;\n-\n-\n---\n--- Name: comment_retraction_id_seq1; Type: SEQUENCE; Schema: public; Owner: snowdrift_development\n---\n-\n-CREATE SEQUENCE comment_retraction_id_seq1\n-    START WITH 1\n-    INCREMENT BY 1\n-    NO MINVALUE\n-    NO MAXVALUE\n-    CACHE 1;\n-\n-\n-ALTER TABLE public.comment_retraction_id_seq1 OWNER TO snowdrift_development;\n-\n---\n--- Name: comment_retraction_id_seq1; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER SEQUENCE comment_retraction_id_seq1 OWNED BY comment_retraction.id;\n+ALTER SEQUENCE comment_retraction_id_seq OWNED BY comment_retraction.id;\n \n \n --\n@@ -678,8 +644,7 @@ CREATE TABLE message (\n     created_ts timestamp without time zone NOT NULL,\n     "from" bigint,\n     "to" bigint,\n-    content character varying NOT NULL,\n-    automated boolean DEFAULT false NOT NULL\n+    content character varying NOT NULL\n );\n \n \n@@ -1028,42 +993,6 @@ ALTER SEQUENCE project_user_role_id_seq OWNED BY project_user_role.id;\n \n \n --\n--- Name: rethread; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: \n---\n-\n-CREATE TABLE rethread (\n-    id integer NOT NULL,\n-    ts timestamp without time zone NOT NULL,\n-    moderator bigint NOT NULL,\n-    old_comment bigint NOT NULL,\n-    reason character varying NOT NULL\n-);\n-\n-\n-ALTER TABLE public.rethread OWNER TO snowdrift_development;\n-\n---\n--- Name: rethread_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development\n---\n-\n-CREATE SEQUENCE rethread_id_seq\n-    START WITH 1\n-    INCREMENT BY 1\n-    NO MINVALUE\n-    NO MAXVALUE\n-    CACHE 1;\n-\n-\n-ALTER TABLE public.rethread_id_seq OWNER TO snowdrift_development;\n-\n---\n--- Name: rethread_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER SEQUENCE rethread_id_seq OWNED BY rethread.id;\n-\n-\n---\n -- Name: role_event; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: \n --\n \n@@ -1612,13 +1541,6 @@ ALTER TABLE ONLY comment_ancestor ALTER COLUMN id SET DEFAULT nextval('comment_a\n -- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY comment_closure ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq'::regclass);\n-\n-\n---\n--- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n---\n-\n ALTER TABLE ONLY comment_rethread ALTER COLUMN id SET DEFAULT nextval('comment_rethread_id_seq'::regclass);\n \n \n@@ -1626,7 +1548,7 @@ ALTER TABLE ONLY comment_rethread ALTER COLUMN id SET DEFAULT nextval('comment_r\n -- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY comment_retraction ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq1'::regclass);\n+ALTER TABLE ONLY comment_retraction ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq'::regclass);\n \n \n --\n@@ -1773,13 +1695,6 @@ ALTER TABLE ONLY project_user_role ALTER COLUMN id SET DEFAULT nextval('project_\n -- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY rethread ALTER COLUMN id SET DEFAULT nextval('rethread_id_seq'::regclass);\n-\n-\n---\n--- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development\n---\n-\n ALTER TABLE ONLY role_event ALTER COLUMN id SET DEFAULT nextval('role_event_id_seq'::regclass);\n \n \n@@ -1917,7 +1832,6 @@ COPY build (id, boot_time, base, diff) FROM stdin;\n 18\t2014-01-24 23:28:23.255958\tb857adcedd7be11cf2909b5d6cb536fb17d999c9\t\n 19\t2014-02-04 05:22:47.977252\te42ac19d7713acb15779eb289dc57697a265ffe3\t\n 20\t2014-03-02 05:31:59.002319\t008e9bc87dbbab3764cfac6ac19bd3db630387d4\tdiff --git a/Import.hs b/Import.hs\\nindex 09eb514..0c29391 100644\\n--- a/Import.hs\\n+++ b/Import.hs\\n@@ -175,3 +175,4 @@ renderBootstrap3 aform fragment = do\\n                 |]\\n     return (res, widget)\\n \\n+\\ndiff --git a/Settings.hs b/Settings.hs\\nindex e303486..ad348e9 100644\\n--- a/Settings.hs\\n+++ b/Settings.hs\\n@@ -57,8 +57,6 @@ widgetFileSettings = def\\n         }\\n     }\\n \\n--- The rest of this file contains settings which rarely need changing by a\\n--- user.\\n \\n widgetFile :: String -> Q Exp\\n widgetFile = (if development then widgetFileReload\\ndiff --git a/Snowdrift.cabal b/Snowdrift.cabal\\nindex 16e0bd9..87f98b3 100644\\n--- a/Snowdrift.cabal\\n+++ b/Snowdrift.cabal\\n@@ -228,3 +228,5 @@ test-suite test\\n                  , network\\n                  , http-types\\n                  , wai-test\\n+                 , unix\\n+                 , mtl\\n\n-21\t2014-05-27 16:23:29.728077\te81380ff27267098517bd582269afd7780a4a517\tdiff --git a/README.md b/README.md\\nindex 25ea3e8..2bbd67e 100644\\n--- a/README.md\\n+++ b/README.md\\n@@ -189,7 +189,9 @@ Then add user to database:\\n \\n     postgres=# grant all privileges on database snowdrift_development to snowdrift_development;\\n \\n-Leave postgres (with ctrl-D), then edit config/postgresql.yml and update the password to match the one you entered.\\n+Leave postgres (with ctrl-D).\\n+\\n+Edit config/postgresql.yml and update the password to match the one you entered.\\n \\n Import development database:\\n \\n@@ -355,7 +357,7 @@ Go to the postgres=# prompt:\\n \\n     sudo -u postgres psql\\n \\n-Unmark the template:\\n+Unmark the template (don't include the postgres=# prompt part):\\n \\n     postgres=# update pg_database set datistemplate=false where datname='snowdrift_test_template';\\n           \\n\n \\.\n \n \n@@ -1925,18 +1839,18 @@ COPY build (id, boot_time, base, diff) FROM stdin;\n -- Name: build_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development\n --\n \n-SELECT pg_catalog.setval('build_id_seq', 21, true);\n+SELECT pg_catalog.setval('build_id_seq', 20, true);\n \n \n --\n -- Data for Name: comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n-COPY comment (id, created_ts, moderated_ts, moderated_by, parent, "user", text, depth, discussion, rethreaded) FROM stdin;\n-1\t2014-01-21 18:11:03.914397\t2014-01-21 18:12:36.696658\t1\t\\N\t1\tThis is a comment.\t0\t2\t\\N\n-2\t2014-01-21 18:13:00.273315\t2014-01-21 18:13:10.464805\t1\t1\t1\tReplies are threaded.\t1\t2\t\\N\n-3\t2014-01-21 18:13:57.732222\t\\N\t\\N\t\\N\t1\tWhen a comment is posted by an unestablished user, it is marked for moderation and only shown to moderators.\t0\t2\t\\N\n-4\t2014-01-21 18:15:30.945499\t2014-01-21 18:15:37.484472\t1\t\\N\t1\tadding a line starting with "ticket:" such as\\n\\nticket: this is a ticket\\n\\nmakes the post show up at /t where all the tickets are listed\t0\t2\t\\N\n+COPY comment (id, created_ts, moderated_ts, moderated_by, parent, "user", text, depth, discussion) FROM stdin;\n+1\t2014-01-21 18:11:03.914397\t2014-01-21 18:12:36.696658\t1\t\\N\t1\tThis is a comment.\t0\t2\n+2\t2014-01-21 18:13:00.273315\t2014-01-21 18:13:10.464805\t1\t1\t1\tReplies are threaded.\t1\t2\n+3\t2014-01-21 18:13:57.732222\t\\N\t\\N\t\\N\t1\tWhen a comment is posted by an unestablished user, it is marked for moderation and only shown to moderators.\t0\t2\n+4\t2014-01-21 18:15:30.945499\t2014-01-21 18:15:37.484472\t1\t\\N\t1\tadding a line starting with "ticket:" such as\\n\\nticket: this is a ticket\\n\\nmakes the post show up at /t where all the tickets are listed\t0\t2\n \\.\n \n \n@@ -1957,14 +1871,6 @@ SELECT pg_catalog.setval('comment_ancestor_id_seq', 1, true);\n \n \n --\n--- Data for Name: comment_closure; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n---\n-\n-COPY comment_closure (id, ts, reason, comment, closed_by, type) FROM stdin;\n-\\.\n-\n-\n---\n -- Name: comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development\n --\n \n@@ -1975,7 +1881,7 @@ SELECT pg_catalog.setval('comment_id_seq', 4, true);\n -- Data for Name: comment_rethread; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n-COPY comment_rethread (id, rethread, old_comment, new_comment) FROM stdin;\n+COPY comment_rethread (id, ts, moderator, old_parent, old_discussion, new_parent, new_discussion, comment, reason) FROM stdin;\n \\.\n \n \n@@ -2002,13 +1908,6 @@ SELECT pg_catalog.setval('comment_retraction_id_seq', 1, false);\n \n \n --\n--- Name: comment_retraction_id_seq1; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development\n---\n-\n-SELECT pg_catalog.setval('comment_retraction_id_seq1', 1, false);\n-\n-\n---\n -- Data for Name: comment_tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n@@ -2043,7 +1942,7 @@ SELECT pg_catalog.setval('committee_user_id_seq', 1, false);\n --\n \n COPY database_version (id, last_migration) FROM stdin;\n-1\t7\n+1\t5\n \\.\n \n \n@@ -2169,8 +2068,8 @@ SELECT pg_catalog.setval('manual_establishment_id_seq', 1, false);\n -- Data for Name: message; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n-COPY message (id, project, created_ts, "from", "to", content, automated) FROM stdin;\n-1\t1\t2014-01-21 22:31:51.496246\t1\t\\N\tWelcome!\tf\n+COPY message (id, project, created_ts, "from", "to", content) FROM stdin;\n+1\t1\t2014-01-21 22:31:51.496246\t1\t\\N\tWelcome!\n \\.\n \n \n@@ -2324,21 +2223,6 @@ SELECT pg_catalog.setval('project_user_role_id_seq', 4, true);\n \n \n --\n--- Data for Name: rethread; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n---\n-\n-COPY rethread (id, ts, moderator, old_comment, reason) FROM stdin;\n-\\.\n-\n-\n---\n--- Name: rethread_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development\n---\n-\n-SELECT pg_catalog.setval('rethread_id_seq', 1, false);\n-\n-\n---\n -- Data for Name: role_event; Type: TABLE DATA; Schema: public; Owner: snowdrift_development\n --\n \n@@ -2616,16 +2500,8 @@ ALTER TABLE ONLY comment_rethread\n -- Name: comment_retraction_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: \n --\n \n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_retraction_pkey PRIMARY KEY (id);\n-\n-\n---\n--- Name: comment_retraction_pkey1; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: \n---\n-\n ALTER TABLE ONLY comment_retraction\n-    ADD CONSTRAINT comment_retraction_pkey1 PRIMARY KEY (id);\n+    ADD CONSTRAINT comment_retraction_pkey PRIMARY KEY (id);\n \n \n --\n@@ -2789,14 +2665,6 @@ ALTER TABLE ONLY project_user_role\n \n \n --\n--- Name: rethread_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: \n---\n-\n-ALTER TABLE ONLY rethread\n-    ADD CONSTRAINT rethread_pkey PRIMARY KEY (id);\n-\n-\n---\n -- Name: role_event_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: \n --\n \n@@ -3115,30 +2983,6 @@ ALTER TABLE ONLY comment_ancestor\n \n \n --\n--- Name: comment_closure_closed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_closure_closed_by_fkey FOREIGN KEY (closed_by) REFERENCES "user"(id);\n-\n-\n---\n--- Name: comment_closure_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_closure_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);\n-\n-\n---\n--- Name: comment_closure_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_closure_user_fkey FOREIGN KEY (closed_by) REFERENCES "user"(id);\n-\n-\n---\n -- Name: comment_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n@@ -3171,43 +3015,51 @@ ALTER TABLE ONLY comment\n \n \n --\n--- Name: comment_rethread_new_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n ALTER TABLE ONLY comment_rethread\n-    ADD CONSTRAINT comment_rethread_new_comment_fkey FOREIGN KEY (new_comment) REFERENCES comment(id);\n+    ADD CONSTRAINT comment_rethread_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);\n \n \n --\n--- Name: comment_rethread_old_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_moderator_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n ALTER TABLE ONLY comment_rethread\n-    ADD CONSTRAINT comment_rethread_old_comment_fkey FOREIGN KEY (old_comment) REFERENCES comment(id);\n+    ADD CONSTRAINT comment_rethread_moderator_fkey FOREIGN KEY (moderator) REFERENCES "user"(id);\n \n \n --\n--- Name: comment_rethread_rethread_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_new_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n ALTER TABLE ONLY comment_rethread\n-    ADD CONSTRAINT comment_rethread_rethread_fkey FOREIGN KEY (rethread) REFERENCES rethread(id);\n+    ADD CONSTRAINT comment_rethread_new_discussion_fkey FOREIGN KEY (new_discussion) REFERENCES discussion(id);\n \n \n --\n--- Name: comment_rethreaded_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_new_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY comment\n-    ADD CONSTRAINT comment_rethreaded_fkey FOREIGN KEY (rethreaded) REFERENCES rethread(id);\n+ALTER TABLE ONLY comment_rethread\n+    ADD CONSTRAINT comment_rethread_new_parent_fkey FOREIGN KEY (new_parent) REFERENCES comment(id);\n \n \n --\n--- Name: comment_retraction_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+-- Name: comment_rethread_old_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \n-ALTER TABLE ONLY comment_closure\n-    ADD CONSTRAINT comment_retraction_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);\n+ALTER TABLE ONLY comment_rethread\n+    ADD CONSTRAINT comment_rethread_old_discussion_fkey FOREIGN KEY (old_discussion) REFERENCES discussion(id);\n+\n+\n+--\n+-- Name: comment_rethread_old_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n+--\n+\n+ALTER TABLE ONLY comment_rethread\n+    ADD CONSTRAINT comment_rethread_old_parent_fkey FOREIGN KEY (old_parent) REFERENCES comment(id);\n \n \n --\n@@ -3483,22 +3335,6 @@ ALTER TABLE ONLY project_user_role\n \n \n --\n--- Name: rethread_moderator_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY rethread\n-    ADD CONSTRAINT rethread_moderator_fkey FOREIGN KEY (moderator) REFERENCES "user"(id);\n-\n-\n---\n--- Name: rethread_old_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n---\n-\n-ALTER TABLE ONLY rethread\n-    ADD CONSTRAINT rethread_old_comment_fkey FOREIGN KEY (old_comment) REFERENCES comment(id);\n-\n-\n---\n -- Name: role_event_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development\n --\n \ndiff --git a/migrations/migrate8 b/migrations/migrate8\ndeleted file mode 100644\nindex d30aabf..0000000\n--- a/migrations/migrate8\n+++ /dev/null\n@@ -1 +0,0 @@\n-ALTER TABLE "message" ADD COLUMN "automated" BOOLEAN NOT NULL DEFAULT False;\n
24	2014-05-27 18:37:11.184342	e81380ff27267098517bd582269afd7780a4a517	diff --git a/README.md b/README.md\nindex 25ea3e8..4c3ddf2 100644\n--- a/README.md\n+++ b/README.md\n@@ -355,7 +355,7 @@ Go to the postgres=# prompt:\n \n     sudo -u postgres psql\n \n-Unmark the template:\n+Unmark the template (don't include the postgres=# prompt part):\n \n     postgres=# update pg_database set datistemplate=false where datname='snowdrift_test_template';\n           \n
25	2014-08-26 00:39:11.954298	2e2563da7d5fb011e0d2a4fa17617caaaa57cbb8	diff --git a/Application.hs b/Application.hs\nindex 4dfdda4..0e7c27b 100644\n--- a/Application.hs\n+++ b/Application.hs\n@@ -9,9 +9,9 @@ module Application\n \n import Import\n import Settings\n+import SnowdriftEventHandler\n import Version\n \n-import           Blaze.ByteString.Builder             (toLazyByteString)\n import           Control.Concurrent                   (forkIO, threadDelay)\n import           Control.Concurrent.STM               (atomically, newTChanIO, tryReadTChan)\n import           Control.Monad.Logger                 (runLoggingT, runStderrLoggingT)\n@@ -20,10 +20,7 @@ import           Control.Monad.Trans.Resource\n import           Data.ByteString                      (ByteString)\n import           Data.Default                         (def)\n import qualified Data.List                            as L\n-import           Data.Maybe                           (fromJust)\n import           Data.Text                            as T\n-import qualified Data.Text.Lazy                       as TL\n-import qualified Data.Text.Lazy.Encoding              as TLE\n import qualified Data.Text.IO                         as T\n import qualified Database.Persist\n import           Database.Persist.Postgresql          (pgConnStr, withPostgresqlConn)\n@@ -37,47 +34,35 @@ import           System.Directory\n import           System.Environment                   (lookupEnv)\n import           System.Log.FastLogger                (newStdoutLoggerSet, defaultBufSize, flushLogStr)\n import           System.Posix.Env.ByteString\n-import           Yesod                                (renderRoute)\n import           Yesod.Core.Types                     (loggerSet, Logger (Logger))\n import           Yesod.Default.Config\n import           Yesod.Default.Handlers\n import           Yesod.Default.Main\n-import           Yesod.Markdown\n \n -- Import all relevant handler modules here.\n -- Don't forget to add new modules to your cabal file!\n \n-import Handler.Application\n-import Handler.Applications\n import Handler.BuildFeed\n-import Handler.Contact\n+import Handler.Comment\n import Handler.Home\n import Handler.HonorPledge\n import Handler.Invitation\n-import Handler.Invite\n import Handler.JsLicense\n import Handler.MarkdownTutorial\n-import Handler.Messages\n+import Handler.Notification\n import Handler.PostLogin\n import Handler.Privacy\n import Handler.Project\n import Handler.RepoFeed\n-import Handler.Tickets\n import Handler.ToU\n import Handler.UpdateShares\n import Handler.User\n-import Handler.UserBalance\n-import Handler.UserPledges\n import Handler.Volunteer\n import Handler.Who\n import Handler.Widget\n import Handler.Wiki\n import Handler.Wiki.Comment\n \n-import Model.Message\n--- import Model.SnowdriftEvent.Internal\n-import Model.User\n-\n import Widgets.Navbar\n \n runSql :: MonadSqlPersist m => Text -> m ()\n@@ -160,10 +145,7 @@ makeFoundation conf = do\n                        dbconf\n                        logger\n                        event_chan\n-                       -- Add more event handlers here.\n-                       [ messageEventHandler\n-                       , eventInserterHandler\n-                       ]\n+                       snowdriftEventHandlers\n \n     -- Perform database migration using our application's logging settings.\n     case appEnv conf of\n@@ -236,20 +218,26 @@ doMigration = do\n         runSql migration\n \n     let new_last_migration = L.maximum $ migration_number : L.map fst migration_files\n+\n     update $ flip set [ DatabaseVersionLastMigration =. val new_last_migration ]\n \n     migrations <- parseMigration' migrateAll\n \n     let (unsafe, safe) = L.partition fst migrations\n \n-    unless (L.null $ L.map snd safe) $ do\n-        let filename = "migrations/migrate" <> show (new_last_migration + 1)\n+    maybe_newer_last_migration <-\n+        if (L.null $ L.map snd safe)\n+         then return Nothing\n+         else do\n+            let filename = "migrations/migrate" <> show (new_last_migration + 1)\n+\n+            liftIO $ T.writeFile filename $ T.unlines $ L.map ((`snoc` ';') . snd) safe\n \n-        liftIO $ T.writeFile filename $ T.unlines $ L.map ((`snoc` ';') . snd) safe\n+            $(logWarn) $ "wrote " <> T.pack (show $ L.length safe) <> " safe statements to " <> T.pack filename\n \n-        $(logWarn) $ "wrote " <> T.pack (show $ L.length safe) <> " safe statements to " <> T.pack filename\n+            mapM_ (runSql . snd) migrations\n \n-        mapM_ (runSql . snd) migrations\n+            return $ Just $ new_last_migration + 1\n \n     unless (L.null $ L.map snd unsafe) $ do\n         let filename = "migrations/migrate.unsafe"\n@@ -260,6 +248,8 @@ doMigration = do\n \n         error "Some migration steps were unsafe.  Aborting."\n \n+    maybe (return ()) (\\ newer_last_migration -> update $ flip set [ DatabaseVersionLastMigration =. val newer_last_migration ]) maybe_newer_last_migration\n+\n     rolloutStagingWikiPages\n \n \n@@ -358,45 +348,3 @@ forkEventHandler app@App{..} = void . forkIO . forever $ do\n         Just event -> do\n             mapM_ (runDaemon app) (appEventHandlers <*> [event])\n             handleNEvents (n-1)\n-\n--- | Handler in charge of sending Messages to interested parties.\n-messageEventHandler :: SnowdriftEvent -> Daemon ()\n-messageEventHandler (ECommentPosted comment_id comment) = case commentParent comment of\n-    Nothing -> return ()\n-    Just parent_comment_id -> do\n-        (parent_user_id, delivery) <- runDB $ do\n-            parent_user_id <- commentUser <$> Database.Persist.getJust parent_comment_id\n-            delivery <- fetchUserMessagePrefDB parent_user_id MessageReply\n-            return (parent_user_id, delivery)\n-        -- Any non-Nothing delivery implies an internal Message should be sent.\n-        when (isJust delivery) $ do\n-            app <- ask\n-            let parent_comment_route = renderRoute' (CommentDirectLinkR parent_comment_id) app\n-                reply_comment_route  = renderRoute' (CommentDirectLinkR comment_id)        app\n-\n-            let content = mconcat\n-                  [ "Someone replied to [your comment]("\n-                  , Markdown parent_comment_route\n-                  , ")! You can view the reply [here]("\n-                  , Markdown reply_comment_route\n-                  , ")."\n-                  , ""\n-                  , "*You can filter these messages by adjusting the settings in your profile.*"\n-                  ]\n-            runSDB $ insertMessage_ MessageReply Nothing Nothing (Just parent_user_id) content True\n-messageEventHandler _ = return ()\n-\n--- | Handler in charge of inserting events (stripped down) into a separate table for each type.\n-eventInserterHandler :: SnowdriftEvent -> Daemon ()\n--- If an unapproved comment is sent as an ECommentPosted event, bad things will happen (fromJust).\n-eventInserterHandler (ECommentPosted comment_id Comment{..})  = runDB (insert_ (EventCommentPosted comment_id (fromJust commentModeratedTs)))\n-eventInserterHandler (ECommentPending comment_id Comment{..}) = runDB (insert_ (EventCommentPending comment_id commentCreatedTs))\n-eventInserterHandler (EMessageSent message_id Message{..})    = runDB (insert_ (EventMessageSent message_id messageCreatedTs))\n-eventInserterHandler (EWikiEdit wiki_edit_id WikiEdit{..})    = runDB (insert_ (EventWikiEdit wiki_edit_id wikiEditTs))\n-\n-renderRoute' :: Route App -> App -> Text\n-renderRoute' route app =\n-    let (path_pieces, query_params) = renderRoute route\n-    -- converting a lazy ByteString to a strict Text... ridiculous!\n-    -- why does joinPath return a ByteString??\n-    in TL.toStrict $ TLE.decodeUtf8 $ toLazyByteString (joinPath app "" path_pieces query_params)\ndiff --git a/Foundation.hs b/Foundation.hs\nindex 76ef362..37d1e25 100644\n--- a/Foundation.hs\n+++ b/Foundation.hs\n@@ -3,7 +3,7 @@ module Foundation where\n import           Model\n import           Model.Currency\n import           Model.Established.Internal         (Established(..))\n-import           Model.Message.Internal             (MessageType(..), MessageDelivery(..))\n+import           Model.Notification.Internal        (NotificationType(..), NotificationDelivery(..))\n import           Model.SnowdriftEvent.Internal\n import qualified Settings\n import           Settings                           (widgetFile, Extra (..))\n@@ -347,8 +347,9 @@ createUser :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Ha\n createUser ident passwd name avatar nick = do\n     now <- liftIO getCurrentTime\n     handle (\\DBException -> return Nothing) $ runYDB $ do\n-        account_id <- insert $ Account 0\n-        user <- maybe return setPassword passwd $ User ident (Just now) Nothing Nothing name account_id avatar Nothing Nothing nick now now EstUnestablished\n+        account_id <- insert (Account 0)\n+        discussion_id <- insert (Discussion 0)\n+        user <- maybe return setPassword passwd $ User ident (Just now) Nothing Nothing name account_id avatar Nothing Nothing nick now now EstUnestablished discussion_id\n         uid_maybe <- insertUnique user\n         Entity snowdrift_id _ <- getBy404 $ UniqueProjectHandle "snowdrift"\n         case uid_maybe of\n@@ -362,22 +363,22 @@ createUser ident passwd name avatar nick = do\n                 forM_ default_tag_colors $ \\ (Entity _ (DefaultTagColor tag color)) -> insert $ TagColor tag user_id color\n                 --\n \n-                insertDefaultMessagePrefs user_id\n+                insertDefaultNotificationPrefs user_id\n \n-                let message_text = Markdown $ T.unlines\n+                let notif_text = Markdown $ T.unlines\n                         [ "Thanks for registering!"\n                         , "<br> Please read our [**welcome message**](/p/snowdrift/w/welcome), and let us know any questions."\n                         ]\n                 -- TODO: change snowdrift_id to the generated site-project id\n-                -- TODO(mitchell): This message doesn't get sent to the event channel. Is that okay?\n-                insert_ $ Message MessageDirect (Just snowdrift_id) now Nothing (Just user_id) message_text True\n+                -- TODO(mitchell): This notification doesn't get sent to the event channel. Is that okay?\n+                insert_ $ Notification now NotifWelcome user_id (Just snowdrift_id) notif_text False\n                 return $ Just user_id\n             Nothing -> do\n                 lift $ addAlert "danger" "E-mail or handle already in use."\n                 throwIO DBException\n   where\n-    insertDefaultMessagePrefs :: UserId -> DB ()\n-    insertDefaultMessagePrefs user_id = insert_ $ UserMessagePref user_id MessageReply DeliverInternal\n+    insertDefaultNotificationPrefs :: UserId -> DB ()\n+    insertDefaultNotificationPrefs user_id = insert_ (UserNotificationPref user_id NotifReply NotifDeliverInternal)\n \n instance YesodJquery App\n \n@@ -416,6 +417,7 @@ addAlertEm level msg em = do\n             #{msg}\n     |] render\n \n+-- TODO(mitchell): don't export this\n addAlert :: Text -> Text -> Handler ()\n addAlert level msg = do\n     render <- getUrlRenderParams\n@@ -427,6 +429,12 @@ addAlert level msg = do\n             #{msg}\n     |] render\n \n+alertDanger, alertInfo, alertSuccess, alertWarning :: Text -> Handler ()\n+alertDanger  = addAlert "danger"\n+alertInfo    = addAlert "info"\n+alertSuccess = addAlert "success"\n+alertWarning = addAlert "warning"\n+\n getAlert :: Handler (Maybe Html)\n getAlert = do\n     mmsg <- liftM (fmap preEscapedToMarkup) $ lookupSession alertKey\ndiff --git a/Handler/Application.hs b/Handler/Application.hs\ndeleted file mode 100644\nindex 0bb7e7b..0000000\n--- a/Handler/Application.hs\n+++ /dev/null\n@@ -1,40 +0,0 @@\n--- Application.hs (singular name) is for viewing the details of a single volunteer's application\n-\n-module Handler.Application where\n-\n-import Import\n-\n-import Model.User\n-\n-\n-\n-import qualified Data.Text as T\n-\n-getApplicationR :: Text -> VolunteerApplicationId -> Handler Html\n-getApplicationR project_handle application_id = do\n-    viewer_id <- requireAuthId\n-    project <- entityVal <$> (runYDB . getBy404 $ UniqueProjectHandle project_handle)\n-\n-    affiliated <- runDB $ (||)\n-        <$> isProjectAffiliated project_handle viewer_id\n-        <*> isProjectAdmin "snowdrift" viewer_id\n-\n-    unless affiliated $ permissionDenied "you must be affiliated with this project to view applications"\n-\n-    (application, user) <- runYDB $ do\n-        application <- get404 application_id\n-        let user_id = volunteerApplicationUser application\n-        user <- get404 user_id\n-        return (application, Entity user_id user)\n-\n-    interests :: [Value Text] <- runDB $ select $ from $ \\ (volunteer_interest `InnerJoin` interest) -> do\n-        on_ $ interest ^. InterestId ==. volunteer_interest ^. VolunteerInterestInterest\n-        where_ $ volunteer_interest ^. VolunteerInterestVolunteer ==. val application_id\n-        return (interest ^. InterestDescription)\n-\n-    let rendered_interests = T.intercalate ", " $ map (\\ (Value x) -> x) interests\n-\n-    defaultLayout $ do\n-        setTitle . toHtml $ projectName project <> " Volunteer Application - " <> userPrintName user <> " | Snowdrift.coop"\n-        $(widgetFile "application")\n-\ndiff --git a/Handler/Applications.hs b/Handler/Applications.hs\ndeleted file mode 100644\nindex 8a2d264..0000000\n--- a/Handler/Applications.hs\n+++ /dev/null\n@@ -1,40 +0,0 @@\n--- Applications.hs (plural) is for viewing the *list* of all volunteer applications for a project\n-\n-module Handler.Applications where\n-\n-import Import\n-\n-import Model.User\n-\n-getApplicationsR :: Text -> Handler Html\n-getApplicationsR project_handle = do\n-    viewer_id <- requireAuthId\n-    now <- liftIO getCurrentTime\n-\n-    -- let applications_map = M.fromListWith (++) $ map (id &&& return) $ applications\n-\n-    affiliated <- runDB $ (||)\n-        <$> isProjectAffiliated project_handle viewer_id\n-        <*> isProjectAdmin "snowdrift" viewer_id\n-\n-    unless affiliated $\n-        permissionDenied "you must be affiliated with this project to view applications"\n-\n-    Entity _ project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle\n-    applications <- runYDB $ do\n-        project_id <- fmap entityKey $ getBy404 $ UniqueProjectHandle project_handle\n-        select $\n-         from $ \\ application -> do\n-         where_ $ application ^. VolunteerApplicationProject ==. val project_id\n-         orderBy [ desc $ application ^. VolunteerApplicationCreatedTs ]\n-         return application\n-\n-    runDB $\n-        update $ \\ user -> do\n-        set user [ UserReadApplications =. val now ]\n-        where_ (user ^. UserId ==. val viewer_id)\n-\n-    defaultLayout $ do\n-        setTitle . toHtml $\n-            projectName project <> " Volunteer Applications | Snowdrift.coop"\n-        $(widgetFile "applications")\ndiff --git a/Handler/Comment.hs b/Handler/Comment.hs\nnew file mode 100644\nindex 0000000..b85f457\n--- /dev/null\n+++ b/Handler/Comment.hs\n@@ -0,0 +1,665 @@\n+-- | Handler functions that are shared among various different\n+-- locations Comments may exist.\n+\n+module Handler.Comment\n+    -- Handlers\n+    ( deleteCommentDirectLinkR\n+    , getCommentDirectLinkR\n+    , getCommentTagR\n+    -- Utils\n+    , MakeCommentActionWidget\n+    , earlierClosuresFromMaybeParentId\n+    , getCommentTags\n+    , getMaxDepth\n+    , getMaxDepthDefault\n+    , getMaxDepthNoLimit\n+    , getProjectCommentAddTag\n+    , makeApproveCommentWidget\n+    , makeCloseCommentWidget\n+    , makeCommentForestWidget\n+    , makeCommentTreeWidget\n+    , makeDeleteCommentWidget\n+    , makeEditCommentWidget\n+    , makeFlagCommentWidget\n+    , makeReplyCommentWidget\n+    , makeRethreadCommentWidget\n+    , makeRetractCommentWidget\n+    , postCommentTag\n+    , postCommentApplyTag\n+    , postCommentCreateTag\n+    , postApproveComment\n+    , postCloseComment\n+    , postDeleteComment\n+    , postEditComment\n+    , postFlagComment\n+    , postNewComment\n+    , postRethreadComment\n+    , postRetractComment\n+    , redirectIfRethreaded\n+    ) where\n+\n+import Import\n+\n+import qualified Data.Tree.Extra                 as Tree\n+import           Handler.Utils\n+import           Model.Comment\n+import           Model.Comment.ActionPermissions\n+import           Model.Comment.HandlerInfo\n+import           Model.Comment.Routes\n+import           Model.Project\n+import           Model.Tag\n+import           Model.User\n+import           View.Comment\n+import           Widgets.Preview\n+import           Widgets.Tag\n+\n+import           Data.Default                    (def)\n+import           Data.Tree                       (Forest, Tree, rootLabel)\n+import qualified Data.Map                        as M\n+import qualified Data.Set                        as S\n+import qualified Data.Text                       as T\n+import           Network.HTTP.Types.Status       (movedPermanently301)\n+import           Yesod.Default.Config            (appRoot)\n+\n+--------------------------------------------------------------------------------\n+-- Utility functions\n+\n+earlierClosuresFromMaybeParentId :: Maybe CommentId -> Handler [CommentClosure]\n+earlierClosuresFromMaybeParentId Nothing  = return []\n+earlierClosuresFromMaybeParentId (Just c) = runDB (fetchCommentAncestorClosuresDB' c)\n+\n+-- | Get the max depth from the "maxdepth" GET param, or 11 (arbitrary) if it doesn't exist.\n+getMaxDepth :: Handler MaxDepth\n+getMaxDepth = getMaxDepthDefault 11\n+\n+-- | Get the max depth from the "maxdepth" GET param, or NoMaxDepth if it doesn't exist.\n+getMaxDepthNoLimit :: Handler MaxDepth\n+getMaxDepthNoLimit = maybe NoMaxDepth MaxDepth <$> runInputGet (iopt intField "maxdepth")\n+\n+-- | Get the max depth from the "maxdepth" GET param, or default to the provided depth.\n+getMaxDepthDefault :: Int -> Handler MaxDepth\n+getMaxDepthDefault n = maybe (MaxDepth n) MaxDepth <$> runInputGet (iopt intField "maxdepth")\n+\n+redirectIfRethreaded :: CommentId -> Handler ()\n+redirectIfRethreaded comment_id = runDB (fetchCommentRethreadDB comment_id) >>= \\case\n+    Nothing -> return ()\n+    Just new_comment_id -> redirectWith movedPermanently301 (CommentDirectLinkR new_comment_id)\n+\n+-- | Make a Comment forest Widget. Also returns the comment forest directly, so that additional\n+-- actions may be taken on the comments (such as marking them all as viewed).\n+makeCommentForestWidget\n+        :: CommentHandlerInfo\n+        -> [Entity Comment]\n+        -> Maybe (Entity User)\n+        -> CommentMods                  -- ^ Comment structure modifications.\n+        -> Handler MaxDepth             -- ^ Max depth getter.\n+        -> Bool                         -- ^ Is this a preview?\n+        -> Widget                       -- ^ Widget to display under root comment.\n+        -> Handler (Widget, Forest (Entity Comment))\n+makeCommentForestWidget\n+        CommentHandlerInfo{..}\n+        roots\n+        mviewer\n+        CommentMods{..}\n+        get_max_depth\n+        is_preview\n+        form_under_root_comment = do\n+    let root_ids = map entityKey roots\n+    (children, user_map, earlier_closures_map, closure_map, ticket_map, flag_map) <- runDB $ do\n+        children <- fetchCommentsDescendantsDB root_ids commentHandlerHasPermission\n+\n+        let all_comments    = roots ++ children\n+            all_comment_ids = map entityKey all_comments\n+\n+        earlier_closures_map <- fetchCommentsAncestorClosuresDB root_ids\n+        user_map             <- entitiesMap <$> fetchUsersInDB (S.toList $ makeCommentUsersSet all_comments)\n+        closure_map          <- makeClosureMapDB all_comment_ids\n+        ticket_map           <- makeTicketMapDB  all_comment_ids\n+        flag_map             <- makeFlagMapDB    all_comment_ids\n+\n+        return (children, user_map, earlier_closures_map, closure_map, ticket_map, flag_map)\n+\n+    max_depth <- get_max_depth\n+\n+    let user_map_with_viewer = (maybe id (onEntity M.insert) mviewer) user_map\n+        comment_forest = Tree.sortForestBy orderingNewestFirst (buildCommentForest roots children)\n+        comment_forest_widget =\n+            forM_ comment_forest $ \\comment_tree -> do\n+                let root_id = entityKey (rootLabel comment_tree)\n+                    earlier_closures = M.findWithDefault [] root_id earlier_closures_map\n+\n+                commentTreeWidget\n+                    form_under_root_comment\n+                    comment_tree\n+                    (entityKey <$> mviewer)\n+                    commentHandlerRoutes\n+                    commentHandlerMakeActionPermissions\n+                    (mod_earlier_closures earlier_closures)\n+                    (mod_user_map         user_map_with_viewer)\n+                    (mod_closure_map      closure_map)\n+                    (mod_ticket_map       ticket_map)\n+                    (mod_flag_map         flag_map)\n+                    is_preview\n+                    max_depth\n+                    0\n+\n+    return (comment_forest_widget, comment_forest)\n+\n+-- | Make a Comment tree Widget. Also returns the comment tree directly, so that additional\n+-- actions may be taken on the comments (such as marking them all as viewed).\n+makeCommentTreeWidget\n+        :: CommentHandlerInfo\n+        -> Entity Comment               -- ^ Root comment.\n+        -> Maybe (Entity User)\n+        -> CommentMods                  -- ^ Comment structure modifications.\n+        -> Handler MaxDepth\n+        -> Bool                         -- ^ Is this a preview?\n+        -> Widget                       -- ^ Widget to display under root comment.\n+        -> Handler (Widget, Tree (Entity Comment))\n+makeCommentTreeWidget a b c d e f g  = do\n+    (widget, [tree]) <- makeCommentForestWidget a [b] c d e f g\n+    return (widget, tree)\n+\n+type MakeCommentActionWidget\n+    = Entity Comment\n+   -> Entity User\n+   -> CommentHandlerInfo\n+   -> CommentMods\n+   -> Handler MaxDepth\n+   -> Bool -- is preview?\n+   -> Handler (Widget, Tree (Entity Comment))\n+\n+-- | Make a comment action widget (close, delete, etc.). Unexported. Call one of\n+-- makeCloseCommentWidget, makeDeleteCommentWidget, etc. directly.\n+makeCommentActionWidget :: (CommentActionPermissions -> Bool) -> Widget -> MakeCommentActionWidget\n+makeCommentActionWidget\n+        can_perform_action\n+        form_widget\n+        comment\n+        user\n+        handler_info\n+        mods\n+        get_max_depth\n+        is_preview = do\n+    ok <- can_perform_action <$> (commentHandlerMakeActionPermissions handler_info) comment\n+    unless ok (permissionDenied "You don't have permission to perform this action.")\n+\n+    makeCommentTreeWidget\n+        handler_info\n+        comment\n+        (Just user)\n+        mods\n+        get_max_depth\n+        is_preview\n+        form_widget\n+\n+makeApproveCommentWidget  :: MakeCommentActionWidget\n+makeCloseCommentWidget    :: MakeCommentActionWidget\n+makeEditCommentWidget     :: MakeCommentActionWidget\n+makeFlagCommentWidget     :: MakeCommentActionWidget\n+makeDeleteCommentWidget   :: MakeCommentActionWidget\n+makeReplyCommentWidget    :: MakeCommentActionWidget\n+makeRethreadCommentWidget :: MakeCommentActionWidget\n+makeRetractCommentWidget  :: MakeCommentActionWidget\n+\n+makeApproveCommentWidget  = makeCommentActionWidget can_approve  approveCommentFormWidget\n+makeCloseCommentWidget    = makeCommentActionWidget can_close    (closeCommentFormWidget Nothing)\n+makeFlagCommentWidget     = makeCommentActionWidget can_flag     (flagCommentFormWidget Nothing Nothing)\n+makeDeleteCommentWidget   = makeCommentActionWidget can_delete   deleteCommentFormWidget\n+makeReplyCommentWidget    = makeCommentActionWidget can_reply    commentReplyFormWidget\n+makeRethreadCommentWidget = makeCommentActionWidget can_rethread rethreadCommentFormWidget\n+makeRetractCommentWidget  = makeCommentActionWidget can_retract  (retractCommentFormWidget Nothing)\n+\n+makeEditCommentWidget\n+        comment\n+        user\n+        comment_handler_info\n+        mods\n+        get_max_depth\n+        is_preview = do\n+    makeCommentActionWidget\n+      can_edit\n+      (editCommentFormWidget (commentText (entityVal comment)))\n+      comment\n+      user\n+      comment_handler_info\n+      mods\n+      get_max_depth\n+      is_preview\n+\n+-- | Handle a GET to a /tag/new URL on a Project's Comment. This is distinct from\n+-- adding a tag on some other Comment, because we want to display the Project's\n+-- existing tags. Permission checking should occur *PRIOR TO* this function.\n+getProjectCommentAddTag :: CommentId -> ProjectId -> UserId -> Handler Html\n+getProjectCommentAddTag comment_id project_id user_id = do\n+    (tag_map, tags, project_tags, other_tags) <- runDB $ do\n+        comment_tags <- fetchCommentCommentTagsDB comment_id\n+        tag_map      <- entitiesMap <$> fetchTagsInDB (map commentTagTag comment_tags)\n+        tags         <- M.findWithDefault [] comment_id <$> buildAnnotatedCommentTagsDB (Just user_id) comment_tags\n+        (project_tags, other_tags) <- getProjectTagList project_id\n+        return (tag_map, tags, project_tags, other_tags)\n+\n+    let filter_tags = filter (\\(Entity t _) -> not $ M.member t tag_map)\n+    (apply_form, _)  <- generateFormPost $ newCommentTagForm (filter_tags project_tags) (filter_tags other_tags)\n+    (create_form, _) <- generateFormPost $ createCommentTagForm\n+\n+    defaultLayout $(widgetFile "new_comment_tag")\n+\n+-- | Handle a POST to a /moderate URL. Permission checking should occur *PRIOR TO* this function.\n+postApproveComment :: UserId -> CommentId -> Comment -> Handler ()\n+postApproveComment user_id comment_id comment = do\n+    runSDB (approveCommentDB user_id comment_id comment)\n+    alertSuccess "comment approved"\n+\n+postCloseComment, postRetractComment :: Entity User -> CommentId -> Comment -> CommentHandlerInfo -> Handler (Maybe (Widget, Widget))\n+postCloseComment   = postClosureComment closeCommentForm   newClosedCommentClosure    can_close\n+postRetractComment = postClosureComment retractCommentForm newRetractedCommentClosure can_retract\n+\n+-- | Handle a POST to a /close or /retract URL.\n+-- Permission checking should occur *PRIOR TO* this function.\n+postClosureComment\n+        :: (Maybe Markdown -> Form Markdown)\n+        -> (UserId -> Markdown -> CommentId -> Handler CommentClosure)\n+        -> (CommentActionPermissions -> Bool)\n+        -> Entity User\n+        -> CommentId\n+        -> Comment\n+        -> CommentHandlerInfo\n+        -> Handler (Maybe (Widget, Widget))\n+postClosureComment\n+        make_closure_form\n+        make_new_comment_closure\n+        can_perform_action\n+        user@(Entity user_id _)\n+        comment_id\n+        comment\n+        comment_handler_info = do\n+    ((result, _), _) <- runFormPost (make_closure_form Nothing)\n+    case result of\n+        FormSuccess reason -> do\n+            new_comment_closure <- make_new_comment_closure user_id reason comment_id\n+            lookupPostMode >>= \\case\n+                Just PostMode -> do\n+                    runDB (insert_ new_comment_closure)\n+                    return Nothing\n+                _ -> do\n+                    (form, _) <- generateFormPost (make_closure_form (Just reason))\n+                    (comment_widget, _) <-\n+                        makeCommentActionWidget\n+                          can_perform_action\n+                          mempty\n+                          (Entity comment_id comment)\n+                          user\n+                          comment_handler_info\n+                          (def { mod_closure_map = M.insert comment_id new_comment_closure })\n+                          (getMaxDepthDefault 0)\n+                          True\n+\n+                    return (Just (comment_widget, form))\n+        _ -> error "Error when submitting form."\n+\n+-- | Handle a POST to a /delete URL. Returns whether or not the Comment was deleted,\n+-- per the "mode" POST param (True = deleted, False = not deleted).\n+-- Permission checking should occur *PRIOR TO* this function.\n+postDeleteComment :: CommentId -> Handler Bool\n+postDeleteComment comment_id =\n+    lookupPostMode >>= \\case\n+        Just PostMode -> do\n+            deleteCommentDirectLinkR comment_id\n+            alertSuccess "comment deleted"\n+            return True\n+        _ -> return False\n+\n+-- | Handle a POST to an /edit URL. Returns Nothing if the comment was edited, or Just Widget\n+-- if there's a preview widget to display (per POST param "mode").\n+-- Permission checking should occur *PRIOR TO* this function.\n+postEditComment :: Entity User -> Entity Comment -> CommentHandlerInfo -> Handler (Maybe Widget)\n+postEditComment user comment@(Entity comment_id _) comment_handler_info = do\n+    ((result, _), _) <- runFormPost (editCommentForm "")\n+    case result of\n+        FormSuccess new_text -> lookupPostMode >>= \\case\n+            Just PostMode -> do\n+                runSYDB (editCommentDB comment_id new_text)\n+                alertSuccess "posted new edit"\n+                return Nothing\n+            _ -> do\n+                (form, _) <- generateFormPost (editCommentForm new_text)\n+                (comment_widget, _) <-\n+                    makeCommentActionWidget\n+                      can_edit\n+                      mempty\n+                      comment\n+                      user\n+                      comment_handler_info\n+                      (def { mod_comment = \\c -> c { commentText = new_text }\n+                           -- Since an edit removes a flagging, don't show the flagged markup in preview.\n+                           , mod_flag_map = M.delete comment_id\n+                           })\n+                      (getMaxDepthDefault 0)\n+                      True\n+                return (Just (previewWidget form "post" comment_widget))\n+        FormMissing -> error "Form missing."\n+        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\\n" msgs)\n+\n+-- | Handle a POST to a /flag URL.\n+-- Permission checking should occur *PRIOR TO* this function.\n+postFlagComment :: Entity User -> Entity Comment -> CommentHandlerInfo -> Handler (Maybe Widget)\n+postFlagComment user@(Entity user_id _) comment@(Entity comment_id _) comment_handler_info = do\n+    ((result, _), _) <- runFormPost (flagCommentForm Nothing Nothing)\n+    case result of\n+        -- TODO(mitchell): Change the form to just return [FlagReason], not Maybe [FlagReason]\n+        FormSuccess (Nothing, _) -> flagFailure "Please check at least one Code of Conduct violation."\n+        FormSuccess (Just [], _) -> flagFailure "Please check at least one Code of Conduct violation."\n+        FormSuccess (Just reasons, message) -> lookupPostMode >>= \\case\n+            Just PostMode -> do\n+                let permalink_route = comment_route_edit (commentHandlerRoutes comment_handler_info) comment_id\n+                permalink_route_text <- getUrlRender <*> pure permalink_route\n+                success <- runSYDB (flagCommentDB comment_id permalink_route_text user_id reasons message)\n+                if success\n+                    then alertSuccess "comment hidden and flagged for revision"\n+                    else alertDanger "error: another user flagged this just before you"\n+                return Nothing\n+            _ -> do\n+                (form, _) <- generateFormPost $ flagCommentForm (Just (Just reasons)) (Just message)\n+\n+                let style_widget =\n+                        -- the CSS below styles this particular flagging submit\n+                        -- button. It would be ideal to have this in a more\n+                        -- generalized place so it can be reused in other flagging\n+                        -- buttons and be in just one place, but this works for\n+                        -- now.\n+                        toWidget [cassius|\n+                            .preview-action-button[type=submit]\n+                                background : dark-red\n+                                background-image : linear-gradient(#ee2700, #bd1000)\n+                                border-color: #a5022a\n+\n+                            .preview-action-button[type=submit]:hover, .preview-action-button[type=submit]:focus, .preview-action-button[type=submit]:active\n+                                background : red\n+                                background-image : linear-gradient(#d22935, #a5022a)\n+                        |]\n+                    form_with_header =\n+                        [whamlet|\n+                            <h4>Code of Conduct Violation(s):\n+                            ^{form}\n+                        |]\n+\n+                (comment_widget, _) <-\n+                    makeCommentActionWidget\n+                      can_flag\n+                      mempty\n+                      comment\n+                      user\n+                      comment_handler_info\n+                      (def { mod_flag_map = M.insert comment_id (message, reasons) })\n+                      (getMaxDepthDefault 0)\n+                      True\n+                return (Just (style_widget <> previewWidget form_with_header "flag comment" comment_widget))\n+\n+        FormFailure errs -> flagFailure (T.intercalate ", " errs)\n+        _ -> flagFailure "Form missing."\n+  where\n+    flagFailure :: Text -> Handler a\n+    flagFailure msg = do\n+        alertDanger msg\n+        Just route <- getCurrentRoute\n+        redirect route\n+\n+-- | Handle a POST to either a /reply or /d URL (reply, or new topic). Checks the POST params for\n+-- "mode" key - could either be a "post" (posts the comment and returns its id) or a "preview"\n+-- (returns the comment tree and form, to wrap in a preview). Permission checking should occur\n+-- *PRIOR TO* this function.\n+postNewComment :: Maybe CommentId -> Entity User -> DiscussionId -> MakeCommentActionPermissions -> Handler (Either CommentId (Widget, Widget))\n+postNewComment mparent_id (Entity user_id user) discussion_id make_permissions = do\n+    -- commentReplyForm is OK here (the alternative is commentNewTopicForm) because they're\n+    -- actually the same form with different titles.\n+    ((result, _), _) <- runFormPost commentReplyForm\n+    case result of\n+        FormSuccess contents -> lookupPostMode >>= \\case\n+            Just PostMode -> do\n+                if userIsEstablished user\n+                    then do\n+                        comment_id <- runSDB (postApprovedCommentDB user_id mparent_id discussion_id contents)\n+                        alertSuccess "comment posted"\n+                        return (Left comment_id)\n+                    else do\n+                        comment_id <- runSDB (postUnapprovedCommentDB user_id mparent_id discussion_id contents)\n+                        alertSuccess "comment submitted for moderation"\n+                        return (Left comment_id)\n+            _ -> do\n+                earlier_closures    <- earlierClosuresFromMaybeParentId mparent_id\n+                depth               <- runDB (fetchCommentDepthFromMaybeParentIdDB mparent_id)\n+                (form, _)           <- generateFormPost (commentForm (maybe "New Topic" (const "Reply") mparent_id) (Just contents))\n+                now                 <- liftIO getCurrentTime\n+\n+                let (approved_ts, approved_by) = if userIsEstablished user\n+                                                       then (Just now, Just user_id)\n+                                                       else (Nothing, Nothing)\n+                    comment = Entity\n+                                (Key $ PersistInt64 0)\n+                                (Comment now approved_ts approved_by discussion_id mparent_id user_id contents depth)\n+\n+                max_depth <- getMaxDepthDefault 0\n+\n+                let comment_tree =\n+                        commentTreeWidget\n+                          mempty\n+                          (Tree.singleton comment)\n+                          (Just user_id)\n+                          dummyCommentRoutes -- 'True' below, so routes aren't used.\n+                          make_permissions\n+                          earlier_closures\n+                          (M.singleton user_id user)\n+                          mempty -- closure map\n+                          mempty -- ticket map - TODO(mitchell): this isn't right... if *this* comment is a ticket, we should display it as such.\n+                          mempty -- flag map\n+                          True\n+                          max_depth\n+                          0\n+\n+                return (Right (comment_tree, form))\n+        FormMissing -> error "Form missing."\n+        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\\n" msgs)\n+\n+postRethreadComment :: UserId -> CommentId -> Comment -> Handler Html\n+postRethreadComment user_id comment_id comment = do\n+    -- TODO(david): AVOID CYCLES\n+\n+    ((result, _), _) <- runFormPost rethreadCommentForm\n+    case result of\n+        FormSuccess (new_parent_url, reason) -> do\n+            app <- getYesod\n+            let splitPath  = drop 1 . T.splitOn "/"\n+                stripQuery = fst . T.break (== '?')\n+                stripRoot  = fromMaybe new_parent_url . T.stripPrefix (appRoot $ settings app)\n+                url        = splitPath $ stripQuery (stripRoot new_parent_url)\n+\n+            let notfound = error "could not find discussion for that URL"\n+\n+            -- FIXME(mitchell,david): We shouldn't have to enumerate the routes like this.\n+            -- Luckily robust rethreading is not priority.\n+            (new_parent_id, new_discussion_id) <- case parseRoute (url, []) of\n+                Just (WikiCommentR new_project_handle new_target new_parent_id) -> do\n+                    new_discussion_id <-\n+                        maybe notfound (wikiPageDiscussion . entityVal) <$>\n+                          runDB (fetchProjectWikiPageByNameDB new_project_handle new_target)\n+                    return (Just new_parent_id, new_discussion_id)\n+                Just (WikiDiscussionR new_project_handle new_target) -> do\n+                    new_discussion_id <-\n+                        maybe notfound (wikiPageDiscussion . entityVal) <$>\n+                          runDB (fetchProjectWikiPageByNameDB new_project_handle new_target)\n+                    return (Nothing, new_discussion_id)\n+                Just (ProjectCommentR new_project_handle new_parent_id) -> do\n+                    new_discussion_id <-\n+                        maybe notfound (projectDiscussion . entityVal) <$>\n+                          runDB (getBy (UniqueProjectHandle new_project_handle))\n+                    return (Just new_parent_id, new_discussion_id)\n+                Just (ProjectDiscussionR new_project_handle) -> do\n+                    new_discussion_id <-\n+                        maybe notfound (projectDiscussion . entityVal) <$>\n+                          runDB (getBy (UniqueProjectHandle new_project_handle))\n+                    return (Nothing, new_discussion_id)\n+\n+                Nothing -> error "failed to parse URL"\n+                _       -> notfound\n+\n+            let old_parent_id = commentParent comment\n+            when (new_parent_id == old_parent_id && new_discussion_id == commentDiscussion comment) $\n+                error "trying to move comment to its current location"\n+\n+            new_parent_depth <- maybe (return $ -1) fetchCommentDepth404DB new_parent_id\n+            old_parent_depth <- maybe (return $ -1) fetchCommentDepth404DB old_parent_id\n+\n+            let depth_offset = old_parent_depth - new_parent_depth\n+\n+            lookupPostMode >>= \\case\n+                Just PostMode -> do\n+                    now <- liftIO getCurrentTime\n+\n+                    runDB $ do\n+                        descendants <- fetchCommentAllDescendantsDB comment_id\n+                        rethread_id <- insert (Rethread now user_id comment_id reason)\n+                        let comments = comment_id : descendants\n+                        new_comment_ids <- rethreadCommentsDB rethread_id depth_offset new_parent_id new_discussion_id comments\n+\n+                        delete $\n+                         from $ \\ca ->\n+                         where_ $ ca ^. CommentAncestorComment `in_` valList comments\n+\n+                        forM_ new_comment_ids $ \\ new_comment_id -> do\n+                            insertSelect $\n+                             from $ \\(c `InnerJoin` ca) -> do\n+                             on_ (c ^. CommentParent ==. just (ca ^. CommentAncestorComment))\n+                             where_ (c ^. CommentId ==. val new_comment_id)\n+                             return (CommentAncestor <# val new_comment_id <&> (ca ^. CommentAncestorAncestor))\n+\n+                            [Value maybe_new_parent_id] <-\n+                                select $\n+                                from $ \\c -> do\n+                                where_ (c ^. CommentId ==. val new_comment_id)\n+                                return (c ^. CommentParent)\n+\n+                            maybe (return ()) (insert_ . CommentAncestor new_comment_id) maybe_new_parent_id\n+\n+                        when (new_discussion_id /= commentDiscussion comment) $\n+                            update $ \\c -> do\n+                            set c [ CommentDiscussion =. val new_discussion_id ]\n+                            where_ (c ^. CommentId `in_` valList descendants)\n+\n+                    redirect new_parent_url\n+\n+                _ -> error "no preview for rethreads yet" -- TODO(david)\n+        _ -> error "Error when submitting form."\n+\n+getCommentTags :: CommentId -> Handler Html\n+getCommentTags comment_id = do\n+    muser_id <- maybeAuthId\n+    tags <- runDB $ M.findWithDefault [] comment_id <$> (fetchCommentCommentTagsDB comment_id >>= buildAnnotatedCommentTagsDB muser_id)\n+    defaultLayout $(widgetFile "tags")\n+\n+postCommentTag :: CommentId -> TagId -> Handler ()\n+postCommentTag comment_id tag_id = do\n+    user_id <- requireAuthId\n+    direction <- lookupPostParam "direction"\n+\n+    let delta = case T.unpack <$> direction of\n+            Just "+" -> 1\n+            Just "-" -> -1\n+            Just "\\215" -> -1\n+            Nothing -> error "direction unset"\n+            Just str -> error $ "unrecognized direction: " ++ str\n+\n+    runDB $ do\n+        maybe_comment_tag_entity <- getBy (UniqueCommentTag comment_id tag_id user_id)\n+        case maybe_comment_tag_entity of\n+            Nothing -> insert_ (CommentTag comment_id tag_id user_id delta)\n+            Just (Entity comment_tag_id comment_tag) -> case commentTagCount comment_tag + delta of\n+                0 -> delete $ from $ \\ ct -> where_ $ ct ^. CommentTagId ==. val comment_tag_id\n+                x -> void $ update $ \\ ct -> do\n+                    set ct [ CommentTagCount =. val x ]\n+                    where_ $ ct ^. CommentTagId ==. val comment_tag_id\n+\n+postCommentApplyTag :: CommentId -> Handler ()\n+postCommentApplyTag comment_id = do\n+    Entity user_id user <- requireAuth\n+\n+    unless (userCanAddTag user) $\n+        permissionDenied "You must be an established user to add tags"\n+\n+    ((result_apply, _), _) <- runFormPost (newCommentTagForm [] [])\n+    case result_apply of\n+        FormSuccess (mproject_tag_ids, mother_tag_ids) -> do\n+            let project_tag_ids = fromMaybe [] mproject_tag_ids\n+                other_tag_ids   = fromMaybe [] mother_tag_ids\n+\n+            ok <- runDB $ do\n+                valid_tags <- fetchTagsInDB (project_tag_ids <> other_tag_ids)\n+                if null valid_tags\n+                    then return False\n+                    else do\n+                        void (insertMany $ fmap (\\(Entity tag_id _) -> CommentTag comment_id tag_id user_id 1) valid_tags)\n+                        return True\n+            unless ok (permissionDenied "Error: Invalid tag ID.")\n+        FormMissing -> error "form missing"\n+        FormFailure errs -> error $ T.unpack $ "Form failed: " <> T.intercalate "; " errs\n+\n+postCommentCreateTag :: CommentId -> Handler ()\n+postCommentCreateTag comment_id = do\n+    Entity user_id user <- requireAuth\n+\n+    unless (userCanAddTag user) $\n+        permissionDenied "You must be an established user to add tags"\n+\n+    ((result_create, _), _) <- runFormPost $ createCommentTagForm\n+    case result_create of\n+        FormSuccess tag_name -> do\n+            tag_exists <- runDB $ getBy (UniqueTag tag_name) >>= \\case\n+                Nothing -> do\n+                    tag_id <- insert $ Tag tag_name\n+                    insert_ (CommentTag comment_id tag_id user_id 1)\n+                    return False\n+                Just _ -> return True\n+            when tag_exists (alertDanger "That tag already exists.")\n+        FormMissing -> error "form missing"\n+        FormFailure errs -> error $ T.unpack $ "Form failed: " <> T.intercalate "; " errs\n+\n+--------------------------------------------------------------------------------\n+-- /\n+\n+getCommentDirectLinkR :: CommentId -> Handler Html\n+getCommentDirectLinkR comment_id = runDB (fetchCommentWikiPageDB comment_id) >>= \\case\n+    -- comment not on a wiki page? right now, there's nowhere else to check\n+    -- TODO(mitchell): does this require constant attention?\n+    Nothing -> notFound\n+    Just (Entity _ page) -> do\n+        project <- runYDB (get404 (wikiPageProject page))\n+        redirect (WikiCommentR (projectHandle project) (wikiPageTarget page) comment_id)\n+\n+deleteCommentDirectLinkR :: CommentId -> Handler ()\n+deleteCommentDirectLinkR comment_id = do\n+    user_id <- requireAuthId\n+    comment <- runYDB (get404 comment_id)\n+\n+    ok <- runDB $ do\n+        can_delete <- userCanDeleteCommentDB user_id (Entity comment_id comment)\n+        if can_delete\n+            then deleteCommentDB comment_id >> return True\n+            else return False\n+    unless ok (permissionDenied "You don't have permission to delete that comment.")\n+\n+--------------------------------------------------------------------------------\n+-- /c/\n+\n+getCommentTagR :: CommentId -> TagId -> Handler Html\n+getCommentTagR comment_id tag_id = do\n+    muser_id <- maybeAuthId\n+    tags <- runDB $ M.findWithDefault [] comment_id <$> (fetchCommentTagCommentTagsDB comment_id tag_id >>= buildAnnotatedCommentTagsDB muser_id)\n+    case tags of\n+        [] -> error "That tag has not been applied to this comment."\n+        [tag] -> renderTag tag\n+        _ -> error "This should never happen."\n+  where\n+    renderTag (AnnotatedTag tag _ _ user_votes) = do\n+        let tag_name = tagName $ entityVal tag\n+        defaultLayout $(widgetFile "tag")\n+\ndiff --git a/Handler/Contact.hs b/Handler/Contact.hs\ndeleted file mode 100644\nindex 3a02004..0000000\n--- a/Handler/Contact.hs\n+++ /dev/null\n@@ -1,37 +0,0 @@\n-module Handler.Contact where\n-\n-import Import\n-\n-import Model.Message\n-import Widgets.Markdown\n-\n-contactForm :: Form Markdown\n-contactForm = renderBootstrap3 $ areq' snowdriftMarkdownField "" Nothing\n-\n-getContactR :: Text -> Handler Html\n-getContactR project_handle = do\n-    (contact_form, _) <- generateFormPost contactForm\n-    Entity _ project <- runYDB $ getBy404 (UniqueProjectHandle project_handle)\n-    defaultLayout $ do\n-        setTitle . toHtml $ "Contact " <> projectName project <> " | Snowdrift.coop"\n-        $(widgetFile "contact")\n-\n-\n-postContactR :: Text -> Handler Html\n-postContactR project_handle = do\n-    maybe_user_id <- maybeAuthId\n-\n-    ((result, _), _) <- runFormPost contactForm\n-\n-    case result of\n-        FormSuccess content -> do\n-            runSYDB $ do\n-                Entity project_id _ <- lift $ getBy404 $ UniqueProjectHandle project_handle\n-                insertMessage_ MessageDirect (Just project_id) maybe_user_id Nothing content False\n-\n-            addAlert "success" "Comment submitted.  Thank you for your input!"\n-\n-        _ -> addAlert "danger" "Error occurred when submitting form."\n-\n-    redirect $ ContactR project_handle\n-\ndiff --git a/Handler/Discussion.hs b/Handler/Discussion.hs\nnew file mode 100644\nindex 0000000..9f6e689\n--- /dev/null\n+++ b/Handler/Discussion.hs\n@@ -0,0 +1,14 @@\n+module Handler.Discussion where\n+\n+import Import\n+\n+import Model.Comment.Sql\n+import Model.Discussion\n+\n+-- | Given a callback that takes a "root comment getter", call the callback with the appropriate\n+-- "root comment getter", by looking for a "state=open" or "state=closed" GET param.\n+getDiscussion :: ((DiscussionId -> ExprCommentCond -> DB [Entity Comment]) -> Handler Html) -> Handler Html\n+getDiscussion callback = lookupGetParam "state" >>= \\case\n+    Just "closed" -> callback fetchDiscussionClosedRootCommentsDB\n+    -- Not "closed"? Just accept anything else as meaning "open".\n+    _             -> callback fetchDiscussionRootCommentsDB\ndiff --git a/Handler/HonorPledge.hs b/Handler/HonorPledge.hs\nindex aeddcff..a0b175e 100644\n--- a/Handler/HonorPledge.hs\n+++ b/Handler/HonorPledge.hs\n@@ -2,11 +2,11 @@ module Handler.HonorPledge where\n \n import Import\n \n-import Model.User (establishUser, isCurUserEligibleEstablish)\n+import Model.User (establishUserDB, curUserIsEligibleEstablish)\n \n getHonorPledgeR :: Handler Html\n getHonorPledgeR = do\n-    is_elig <- isCurUserEligibleEstablish\n+    is_elig <- curUserIsEligibleEstablish\n     defaultLayout $ do\n         setTitle "Honor Pledge | Snowdrift.coop"\n         $(widgetFile "honor-pledge")\n@@ -16,7 +16,7 @@ postHonorPledgeR = do\n     Entity user_id user <- requireAuth\n     case userEstablished user of\n         EstEligible elig_time reason -> do\n-            runDB $ establishUser user_id elig_time reason\n+            runDB $ establishUserDB user_id elig_time reason\n             setMessage "Congratulations, you are now a fully established user!"\n             redirect HomeR\n         _ -> error "You're not eligible for establishment."\ndiff --git a/Handler/Invite.hs b/Handler/Invite.hs\ndeleted file mode 100644\nindex 73c2208..0000000\n--- a/Handler/Invite.hs\n+++ /dev/null\n@@ -1,106 +0,0 @@\n-{- Each project can generate invitation codes to give users special roles such as Moderator or Team Member or Admin.\n-Invite.hs is the part where users affiliated with a project generate invitations and see outstanding or past invitations. -}\n-\n-module Handler.Invite where\n-\n-import Import\n-\n-import System.Random\n-import Text.Printf\n-import Data.Text (pack, unpack)\n-import qualified Data.Map as M\n-import qualified Data.Set as S\n-\n-import Model.Role\n-import Model.User\n-\n-\n-\n-\n-inviteForm :: Form (Text, Role)\n-inviteForm = renderBootstrap3 $ (,)\n-    <$> areq' textField "About this invitation:" Nothing\n-    <*> areq roleField "Type of Invite:" (Just TeamMember)\n-\n-getInviteR :: Text -> Handler Html\n-getInviteR project_handle = do\n-    viewer_id <- requireAuthId\n-    admin <- runDB $ (||)\n-        <$> isProjectAdmin project_handle viewer_id\n-        <*> isProjectAdmin "snowdrift" viewer_id\n-\n-    unless admin $ permissionDenied "must be an admin to invite"\n-\n-    project <- entityVal <$> (runYDB . getBy404 $ UniqueProjectHandle project_handle)\n-\n-    now <- liftIO getCurrentTime\n-    maybe_invite_code <- lookupSession "InviteCode"\n-    maybe_invite_role <- fmap (read . unpack) <$> lookupSession "InviteRole"\n-    deleteSession "InviteCode"\n-    deleteSession "InviteRole"\n-    let maybe_link = InvitationR project_handle <$> maybe_invite_code\n-    (invite_form, _) <- generateFormPost inviteForm\n-\n-    outstanding_invites <- runDB $\n-        select $\n-        from $ \\ invite -> do\n-        where_ ( invite ^. InviteRedeemed ==. val False )\n-        orderBy [ desc (invite ^. InviteCreatedTs) ]\n-        return invite\n-\n-    redeemed_invites <- runDB $\n-        select $\n-        from $ \\ invite -> do\n-        where_ ( invite ^. InviteRedeemed ==. val True )\n-        orderBy [ desc (invite ^. InviteCreatedTs) ]\n-        limit 20\n-        return invite\n-\n-    let redeemed_users = S.fromList $ mapMaybe (inviteRedeemedBy . entityVal) redeemed_invites\n-        redeemed_inviters = S.fromList $ map (inviteUser . entityVal) redeemed_invites\n-        outstanding_inviters = S.fromList $ map (inviteUser . entityVal) outstanding_invites\n-        user_ids = S.toList $ redeemed_users `S.union` redeemed_inviters `S.union` outstanding_inviters\n-\n-    user_entities <- runDB $ selectList [ UserId <-. user_ids ] []\n-\n-    let users = M.fromList $ map (entityKey &&& id) user_entities\n-\n-    let format_user Nothing = "NULL"\n-        format_user (Just user_id) =\n-            let Entity _ user = users M.! user_id\n-             in fromMaybe (userIdent user) $ userName user\n-\n-        format_inviter user_id =\n-            userPrintName $ users M.! user_id\n-\n-    defaultLayout $ do\n-        setTitle . toHtml $ projectName project <> " - Send Invite | Snowdrift.coop"\n-        $(widgetFile "invite")\n-\n-\n-postInviteR :: Text -> Handler Html\n-postInviteR project_handle = do\n-    user_id <- requireAuthId\n-\n-    admin <- runDB $ (||)\n-        <$> isProjectAdmin project_handle user_id\n-        <*> isProjectAdmin "snowdrift" user_id\n-\n-    unless admin $ permissionDenied "must be an admin to invite"\n-\n-    now <- liftIO getCurrentTime\n-    invite <- liftIO randomIO\n-    project_id <- fmap entityKey $ runYDB $ getBy404 $ UniqueProjectHandle project_handle\n-\n-    ((result, _), _) <- runFormPost inviteForm\n-    case result of\n-        FormSuccess (tag, role) -> do\n-            let invite_code = pack $ printf "%016x" (invite :: Int64)\n-            _ <- runDB $ insert $ Invite now project_id invite_code user_id role tag False Nothing Nothing\n-            setSession "InviteCode" invite_code\n-            setSession "InviteRole" (pack $ show role)\n-\n-        _ -> addAlert "danger" "Error in submitting form."\n-\n-    redirect $ InviteR project_handle\n-\ndiff --git a/Handler/Messages.hs b/Handler/Messages.hs\ndeleted file mode 100644\nindex b3512a2..0000000\n--- a/Handler/Messages.hs\n+++ /dev/null\n@@ -1,47 +0,0 @@\n-module Handler.Messages where\n-\n-import Import\n-\n-import Model.User\n-\n-import qualified Data.Map as M\n-\n-import Model.Project\n-\n-import Widgets.Time\n-\n-getMessagesR :: Handler Html\n-getMessagesR = do\n-    Entity viewer_id viewer <- requireAuth\n-    now <- liftIO getCurrentTime\n-\n-    messages <-\n-        -- TODO: filter by projects? Also, generalize so project-affiliates\n-        -- see any message for their respective projects.\n-        runDB $ do\n-            snowdrift_member <- isProjectAffiliated "snowdrift" viewer_id\n-            select $ from $ \\ message -> do\n-                where_ $ if snowdrift_member\n-                    then message ^. MessageTo ==. val (Just viewer_id) ||. isNothing (message ^. MessageTo)\n-                    else message ^. MessageTo ==. val (Just viewer_id)\n-                orderBy [ desc $ message ^. MessageCreatedTs ]\n-                return message\n-\n-    users <- runDB $ select $ from $ \\ user -> do\n-        where_ (user ^. UserId `in_` valList (mapMaybe (messageFrom . entityVal) messages))\n-        return user\n-\n-    let user_map = M.fromList $ ((viewer_id, viewer):) $ map (entityKey &&& entityVal) users\n-        getUserName user_id =\n-            let user = user_map M.! user_id\n-             in fromMaybe (userIdent user) (userName user)\n-\n-    _ <- runDB $ update $ \\ user -> do\n-        set user [ UserReadMessages =. val now ]\n-        where_ ( user ^. UserId ==. val viewer_id )\n-\n-\n-    defaultLayout $ do\n-        setTitle "Messages | Snowdrift.coop"\n-        $(widgetFile "messages")\n-\ndiff --git a/Handler/Notification.hs b/Handler/Notification.hs\nnew file mode 100644\nindex 0000000..da7d40b\n--- /dev/null\n+++ b/Handler/Notification.hs\n@@ -0,0 +1,35 @@\n+module Handler.Notification where\n+\n+import Import\n+\n+import           Model.Notification\n+import           Model.Project\n+import           Model.User\n+import           Widgets.Time\n+\n+getNotificationsR :: Handler Html\n+getNotificationsR = do\n+    user_id <- requireAuthId\n+    notifs <- runDB $ do\n+        userReadNotificationsDB user_id\n+        fetchUserNotificationsDB user_id\n+    defaultLayout $ do\n+        setTitle "Notifications | Snowdrift.coop"\n+        $(widgetFile "notifications")\n+\n+getArchivedNotificationsR :: Handler Html\n+getArchivedNotificationsR = do\n+    user_id <- requireAuthId\n+    notifs <- runDB (fetchUserArchivedNotificationsDB user_id)\n+    defaultLayout $ do\n+        setTitle "Notifications | Snowdrift.coop"\n+        $(widgetFile "notifications")\n+\n+postArchiveNotificationR :: NotificationId -> Handler ()\n+postArchiveNotificationR notif_id = do\n+    user_id <- requireAuthId\n+    runYDB $ do\n+        notif <- get404 notif_id\n+        unless (user_id == notificationTo notif) $\n+            lift (permissionDenied "You can't archive this notification.")\n+        archiveNotificationDB notif_id\ndiff --git a/Handler/Project.hs b/Handler/Project.hs\nindex b2bec3a..4561b12 100644\n--- a/Handler/Project.hs\n+++ b/Handler/Project.hs\n@@ -4,59 +4,188 @@ module Handler.Project where\n \n import Import\n \n-import Model.Currency\n+import Data.Filter\n+import Data.Order\n+import Handler.Comment\n+import Handler.Discussion\n+import Handler.Utils\n+import Model.Application\n+import Model.Comment\n+import Model.Comment.ActionPermissions\n+import Model.Comment.HandlerInfo\n+import Model.Comment.Sql\n import Model.Discussion\n-import Model.Project\n-import Model.Shares\n+import Model.Issue\n import Model.Markdown\n import Model.Markdown.Diff\n+import Model.Project\n+import Model.Role\n import Model.SnowdriftEvent\n import Model.User\n-import Model.WikiPage\n+import Model.Wiki\n+import View.Comment\n import View.PledgeButton\n+import View.Project\n import View.SnowdriftEvent\n-import Widgets.Markdown\n import Widgets.Preview\n import Widgets.Time\n \n-import           Data.List       (sortBy)\n-import qualified Data.Map        as M\n-import           Data.Maybe      (fromJust, maybeToList)\n-import qualified Data.Text       as T\n-import           Data.Time.Clock\n-import qualified Data.Set        as S\n-import           Yesod.Markdown\n+import           Data.Default  (def)\n+import           Data.List     (sortBy)\n+import qualified Data.Map      as M\n+import           Data.Maybe    (maybeToList)\n+import qualified Data.Set      as S\n+import qualified Data.Text     as T\n+import           Data.Tree     (Forest, Tree)\n+import qualified Data.Tree     as Tree\n+import           System.Random (randomIO)\n+import           Text.Cassius  (cassiusFile)\n+import           Text.Printf\n+\n+--------------------------------------------------------------------------------\n+-- Utility functions\n \n lookupGetParamDefault :: Read a => Text -> a -> Handler a\n-lookupGetParamDefault name def = do\n+lookupGetParamDefault name def_val = do\n     maybe_value <- lookupGetParam name\n-    return $ fromMaybe def $ maybe_value >>= readMaybe . T.unpack\n+    return (fromMaybe def_val (maybe_value >>= readMaybe . T.unpack))\n+\n+-- | Require any of the given Roles, failing with permissionDenied if none are satisfied.\n+requireRolesAny :: [Role] -> Text -> Text -> Handler (UserId, Entity Project)\n+requireRolesAny roles project_handle err_msg = do\n+    user_id <- requireAuthId\n+\n+    (project, ok) <- runYDB $ do\n+        project@(Entity project_id _) <- getBy404 (UniqueProjectHandle project_handle)\n+\n+        ok <- userHasRolesAnyDB roles user_id project_id\n+\n+        return (project, ok)\n+\n+    unless ok $\n+        permissionDenied err_msg\n+\n+    return (user_id, project)\n+\n+-- | Sanity check for Project Comment pages. Redirects if the comment was rethreaded.\n+-- 404's if the comment doesn't exist. 403 if permission denied.\n+checkComment :: Text -> CommentId -> Handler (Maybe (Entity User), Entity Project, Comment)\n+checkComment project_handle comment_id = do\n+    muser <- maybeAuth\n+    (project, comment) <- checkComment' (entityKey <$> muser) project_handle comment_id\n+    return (muser, project, comment)\n+\n+-- | Like checkComment, but authentication is required.\n+checkCommentRequireAuth :: Text -> CommentId -> Handler (Entity User, Entity Project, Comment)\n+checkCommentRequireAuth project_handle comment_id = do\n+    user@(Entity user_id _) <- requireAuth\n+    (project, comment) <- checkComment' (Just user_id) project_handle comment_id\n+    return (user, project, comment)\n+\n+-- | Abstract checkComment and checkCommentRequireAuth. You shouldn't use this function directly.\n+checkComment' :: Maybe UserId -> Text -> CommentId -> Handler (Entity Project, Comment)\n+checkComment' muser_id project_handle comment_id = do\n+    redirectIfRethreaded comment_id\n+\n+    (project, ecomment) <- runYDB $ do\n+        project@(Entity project_id _) <- getBy404 (UniqueProjectHandle project_handle)\n+        let has_permission = exprCommentProjectPermissionFilter muser_id (val project_id)\n+        ecomment <- fetchCommentDB comment_id has_permission\n+        return (project, ecomment)\n+\n+    case ecomment of\n+        Left CommentNotFound         -> notFound\n+        Left CommentPermissionDenied -> permissionDenied "You don't have permission to view this comment."\n+        Right comment                ->\n+            if commentDiscussion comment /= projectDiscussion (entityVal project)\n+                then notFound\n+                else return (project, comment)\n+\n+checkProjectCommentActionPermission :: (CommentActionPermissions -> Bool) -> Text -> Entity Comment -> Handler ()\n+checkProjectCommentActionPermission can_perform_action project_handle comment = do\n+    ok <- can_perform_action <$> makeProjectCommentActionPermissions project_handle comment\n+    unless ok (permissionDenied "You don't have permission to perform this action.")\n+\n+makeProjectCommentForestWidget\n+        :: Maybe (Entity User)\n+        -> ProjectId\n+        -> Text\n+        -> [Entity Comment]\n+        -> CommentMods\n+        -> Handler MaxDepth\n+        -> Bool\n+        -> Widget\n+        -> Handler (Widget, Forest (Entity Comment))\n+makeProjectCommentForestWidget\n+        muser\n+        project_id\n+        project_handle\n+        comments\n+        comment_mods\n+        get_max_depth\n+        is_preview\n+        widget_under_root_comment = do\n+    makeCommentForestWidget\n+      (projectCommentHandlerInfo (entityKey <$> muser) project_id project_handle)\n+      comments\n+      muser\n+      comment_mods\n+      get_max_depth\n+      is_preview\n+      widget_under_root_comment\n+\n+makeProjectCommentTreeWidget\n+        :: Maybe (Entity User)\n+        -> ProjectId\n+        -> Text\n+        -> Entity Comment\n+        -> CommentMods\n+        -> Handler MaxDepth\n+        -> Bool\n+        -> Widget\n+        -> Handler (Widget, Tree (Entity Comment))\n+makeProjectCommentTreeWidget a b c d e f g h = do\n+    (widget, [tree]) <- makeProjectCommentForestWidget a b c [d] e f g h\n+    return (widget, tree)\n+\n+makeProjectCommentActionWidget\n+        :: MakeCommentActionWidget\n+        -> Text\n+        -> CommentId\n+        -> CommentMods\n+        -> Handler MaxDepth\n+        -> Handler (Widget, Tree (Entity Comment))\n+makeProjectCommentActionWidget make_comment_action_widget project_handle comment_id mods get_max_depth = do\n+    (user@(Entity user_id _), Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id\n+    make_comment_action_widget\n+      (Entity comment_id comment)\n+      user\n+      (projectCommentHandlerInfo (Just user_id) project_id project_handle)\n+      mods\n+      get_max_depth\n+      False\n+\n+-------------------------------------------------------------------------------\n+--\n \n getProjectsR :: Handler Html\n getProjectsR = do\n-    projects <- runDB getAllProjects\n-\n+    projects <- runDB fetchAllProjectsDB\n     defaultLayout $ do\n         setTitle "Projects | Snowdrift.coop"\n         $(widgetFile "projects")\n \n-getProjectPledgeButtonR :: Text -> Handler TypedContent\n-getProjectPledgeButtonR project_handle = do\n-   pledges <- runYDB $ do\n-        Entity project_id _project <- getBy404 $ UniqueProjectHandle project_handle\n-        getProjectShares project_id\n-   let png = overlayImage blankPledgeButton $\n-        fillInPledgeCount (fromIntegral (length pledges))\n-   respond "image/png" png\n+--------------------------------------------------------------------------------\n+-- /\n \n getProjectR :: Text -> Handler Html\n getProjectR project_handle = do\n-    maybe_viewer_id <- maybeAuthId\n+    mviewer_id <- maybeAuthId\n \n     (project_id, project, pledges, pledge) <- runYDB $ do\n         Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle\n         pledges <- getProjectShares project_id\n-        pledge <- case maybe_viewer_id of\n+        pledge <- case mviewer_id of\n             Nothing -> return Nothing\n             Just viewer_id -> getBy $ UniquePledge viewer_id project_id\n \n@@ -66,79 +195,548 @@ getProjectR project_handle = do\n         setTitle . toHtml $ projectName project <> " | Snowdrift.coop"\n         renderProject (Just project_id) project pledges pledge\n \n+postProjectR :: Text -> Handler Html\n+postProjectR project_handle = do\n+    (viewer_id, Entity project_id project) <-\n+        requireRolesAny [Admin] project_handle "You do not have permission to edit this project."\n+\n+    ((result, _), _) <- runFormPost $ editProjectForm Nothing\n+\n+    now <- liftIO getCurrentTime\n+\n+    case result of\n+        FormSuccess (UpdateProject name description tags github_repo) ->\n+            lookupPostMode >>= \\case\n+                Just PostMode -> do\n+                    runDB $ do\n+                        when (projectDescription project /= description) $ do\n+                            project_update <- insert $ ProjectUpdate now project_id viewer_id $ diffMarkdown (projectDescription project) description\n+                            last_update <- getBy $ UniqueProjectLastUpdate project_id\n+                            case last_update of\n+                                Just (Entity key _) -> repsert key $ ProjectLastUpdate project_id project_update\n+                                Nothing -> void $ insert $ ProjectLastUpdate project_id project_update\n+\n+                        update $ \\ p -> do\n+                            set p [ ProjectName =. val name, ProjectDescription =. val description, ProjectGithubRepo =. val github_repo ]\n+                            where_ (p ^. ProjectId ==. val project_id)\n+\n+                        tag_ids <- forM tags $ \\ tag_name -> do\n+                            tag_entity_list <- select $ from $ \\ tag -> do\n+                                where_ (tag ^. TagName ==. val tag_name)\n+                                return tag\n+\n+                            case tag_entity_list of\n+                                [] -> insert $ Tag tag_name\n+                                Entity tag_id _ : _ -> return tag_id\n+\n+\n+                        delete $\n+                         from $ \\pt ->\n+                         where_ (pt ^. ProjectTagProject ==. val project_id)\n+\n+                        forM_ tag_ids $ \\tag_id -> insert (ProjectTag project_id tag_id)\n+\n+                    alertSuccess "project updated"\n+                    redirect $ ProjectR project_handle\n+\n+                _ -> do\n+                    let preview_project = project { projectName = name, projectDescription = description, projectGithubRepo = github_repo }\n+\n+                    (form, _) <- generateFormPost $ editProjectForm (Just (preview_project, tags))\n+                    defaultLayout $ previewWidget form "update" $ renderProject (Just project_id) preview_project [] Nothing\n+\n+        x -> do\n+            alertDanger (T.pack $ show x)\n+            redirect (ProjectR project_handle)\n+\n+--------------------------------------------------------------------------------\n+-- /application\n+\n+getApplicationsR :: Text -> Handler Html\n+getApplicationsR project_handle = do\n+    viewer_id <- requireAuthId\n+\n+    (project, applications) <- runYDB $ do\n+        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)\n+        ok <- userIsAffiliatedWithProjectDB viewer_id project_id\n+        unless ok $\n+            lift (permissionDenied "You don't have permission to view this page.")\n+\n+        applications <- fetchProjectVolunteerApplicationsDB project_id\n+        userReadVolunteerApplicationsDB viewer_id\n+        return (project, applications)\n+\n+    defaultLayout $ do\n+        setTitle . toHtml $ projectName project <> " Volunteer Applications | Snowdrift.coop"\n+        $(widgetFile "applications")\n+\n+getApplicationR :: Text -> VolunteerApplicationId -> Handler Html\n+getApplicationR project_handle application_id = do\n+    viewer_id <- requireAuthId\n+    (project, user, application, interests, num_interests) <- runYDB $ do\n+        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)\n+        ok <- userIsAffiliatedWithProjectDB viewer_id project_id\n+        unless ok $\n+            lift (permissionDenied "You don't have permission to view this page.")\n+\n+        application <- get404 application_id\n+        let user_id = volunteerApplicationUser application\n+        user <- get404 user_id\n+        (interests, num_interests) <- (T.intercalate ", " &&& length) <$> fetchApplicationVolunteerInterestsDB application_id\n+        return (project, Entity user_id user, application, interests, num_interests)\n+\n+    defaultLayout $ do\n+        setTitle . toHtml $ projectName project <> " Volunteer Application - " <> userDisplayName user <> " | Snowdrift.coop"\n+        $(widgetFile "application")\n+\n+--------------------------------------------------------------------------------\n+-- /button.png\n+\n+getProjectPledgeButtonR :: Text -> Handler TypedContent\n+getProjectPledgeButtonR project_handle = do\n+   pledges <- runYDB $ do\n+        Entity project_id _project <- getBy404 $ UniqueProjectHandle project_handle\n+        getProjectShares project_id\n+   let png = overlayImage blankPledgeButton $\n+        fillInPledgeCount (fromIntegral (length pledges))\n+   respond "image/png" png\n+\n+--------------------------------------------------------------------------------\n+-- /b\n+\n+getProjectBlogR :: Text -> Handler Html\n+getProjectBlogR project_handle = do\n+    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"\n+    post_count <- fromMaybe 10 <$> fmap (read . T.unpack) <$> lookupGetParam "from"\n+    Entity project_id project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle\n+\n+    let apply_offset blog = maybe id (\\ from_blog rest -> blog ^. ProjectBlogId >=. val from_blog &&. rest) maybe_from\n+\n+    (posts, next) <- fmap (splitAt post_count) $ runDB $\n+        select $\n+        from $ \\blog -> do\n+        where_ $ apply_offset blog $ blog ^. ProjectBlogProject ==. val project_id\n+        orderBy [ desc $ blog ^. ProjectBlogTime, desc $ blog ^. ProjectBlogId ]\n+        limit (fromIntegral post_count + 1)\n+        return blog\n+\n+    renderRouteParams <- getUrlRenderParams\n+\n+    let nextRoute next_id = renderRouteParams (ProjectBlogR project_handle) [("from", toPathPiece next_id)]\n+\n+    defaultLayout $ do\n+        setTitle . toHtml $ projectName project <> " Blog | Snowdrift.coop"\n+\n+        $(widgetFile "project_blog")\n+\n+\n+getNewProjectBlogPostR :: Text -> Handler Html\n+getNewProjectBlogPostR project_handle = do\n+    (_, Entity _ project) <- requireRolesAny [Admin, TeamMember] project_handle "You do not have permission to post to this project's blog."\n \n-renderProject :: Maybe ProjectId\n-              -> Project\n-              -> [Int64]\n-              -> Maybe (Entity Pledge)\n-              -> WidgetT App IO ()\n-renderProject maybe_project_id project pledges pledge = do\n-    let share_value = projectShareValue project\n-        users = fromIntegral $ length pledges\n-        shares = sum pledges\n-        project_value = share_value $* fromIntegral shares\n-        description = markdownWidget (projectHandle project) $ projectDescription project\n+    (blog_form, _) <- generateFormPost $ projectBlogForm Nothing\n+\n+    defaultLayout $ do\n+        setTitle . toHtml $ "Post To " <> projectName project <> " Blog | Snowdrift.coop"\n+\n+        $(widgetFile "new_blog_post")\n \n-        maybe_shares = pledgeShares . entityVal <$> pledge\n+\n+postNewProjectBlogPostR :: Text -> Handler Html\n+postNewProjectBlogPostR project_handle = do\n+    (viewer_id, Entity project_id _) <-\n+        requireRolesAny [Admin, TeamMember] project_handle "You do not have permission to post to this project's blog."\n \n     now <- liftIO getCurrentTime\n \n-    amounts <- case projectLastPayday project of\n-        Nothing -> return Nothing\n-        Just last_payday -> handlerToWidget $ runDB $ do\n-            -- This assumes there were transactions associated with the last payday\n-            [Value (Just last) :: Value (Maybe Rational)] <-\n-                select $\n-                from $ \\ transaction -> do\n-                where_ $\n-                    transaction ^. TransactionPayday ==. val (Just last_payday) &&.\n-                    transaction ^. TransactionCredit ==. val (Just $ projectAccount project)\n-                return $ sum_ $ transaction ^. TransactionAmount\n+    ((result, _), _) <- runFormPost $ projectBlogForm Nothing\n \n-            [Value (Just year) :: Value (Maybe Rational)] <-\n-                select $\n-                from $ \\ (transaction `InnerJoin` payday) -> do\n-                where_ $\n-                    payday ^. PaydayDate >. val (addUTCTime (-365 * 24 * 60 * 60) now) &&.\n-                    transaction ^. TransactionCredit ==. val (Just $ projectAccount project)\n-                on_ $ transaction ^. TransactionPayday ==. just (payday ^. PaydayId)\n-                return $ sum_ $ transaction ^. TransactionAmount\n+    case result of\n+        FormSuccess mk_blog_post -> do\n+            mode <- lookupPostParam "mode"\n+            let action :: Text = "post"\n+            case mode of\n+                Just "preview" -> do\n+                    let blog_post :: ProjectBlog\n+                        blog_post = mk_blog_post now viewer_id project_id (Key $ PersistInt64 0)\n+                        title = projectBlogTitle blog_post\n+                        handle = projectBlogHandle blog_post\n+                        top_content = projectBlogTopContent blog_post\n+                        bottom_content = fromMaybe "" $ projectBlogBottomContent blog_post\n+                        content = top_content <> bottom_content\n \n-            [Value (Just total) :: Value (Maybe Rational)] <-\n-                select $\n-                from $ \\ transaction -> do\n-                where_ $ transaction ^. TransactionCredit ==. val (Just $ projectAccount project)\n-                return $ sum_ $ transaction ^. TransactionAmount\n+                    (form, _) <- generateFormPost $ projectBlogForm $ Just (title, handle, content)\n \n-            return $ Just (Milray $ round last, Milray $ round year, Milray $ round total)\n+                    defaultLayout $ previewWidget form action $ renderBlogPost project_handle blog_post\n \n+                Just x | x == action -> do\n+                    void $ runDB $ do\n+                        discussion_id <- insert $ Discussion 0\n+\n+                        let blog_post :: ProjectBlog\n+                            blog_post = mk_blog_post now viewer_id project_id discussion_id\n+\n+                        insert blog_post\n+\n+                    alertSuccess "posted"\n+                    redirect $ ProjectBlogR project_handle\n+\n+                x -> do\n+                    addAlertEm "danger" ("unrecognized mode: " <> T.pack (show x)) "Error: "\n+                    redirect $ NewProjectBlogPostR project_handle\n+\n+        x -> do\n+            alertDanger $ T.pack $ show x\n+            redirect $ NewProjectBlogPostR project_handle\n+\n+\n+getProjectBlogPostR :: Text -> Text -> Handler Html\n+getProjectBlogPostR project_handle blog_post_handle = do\n+    (project, blog_post) <- runYDB $ do\n+        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle\n+        Entity _ blog_post <- getBy404 $ UniqueProjectBlogPost project_id blog_post_handle\n \n-    ((_, update_shares), _) <- handlerToWidget $ generateFormGet $ maybe previewPledgeForm pledgeForm maybe_project_id\n+        return (project, blog_post)\n \n-    $(widgetFile "project")\n+    defaultLayout $ do\n+        setTitle . toHtml $ projectName project <> " Blog - " <> projectBlogTitle blog_post <> " | Snowdrift.coop"\n \n+        renderBlogPost project_handle blog_post\n+\n+--------------------------------------------------------------------------------\n+-- /c/#CommentId\n+\n+getProjectCommentR :: Text -> CommentId -> Handler Html\n+getProjectCommentR project_handle comment_id = do\n+    (muser, Entity project_id _, comment) <- checkComment project_handle comment_id\n+    (widget, comment_tree) <-\n+        makeProjectCommentTreeWidget\n+          muser\n+          project_id\n+          project_handle\n+          (Entity comment_id comment)\n+          def\n+          getMaxDepth\n+          False\n+          mempty\n+\n+    case muser of\n+        Nothing -> return ()\n+        Just (Entity user_id _) ->\n+            runDB (userMaybeViewProjectCommentsDB user_id project_id (map entityKey (Tree.flatten comment_tree)))\n+\n+    defaultLayout $(widgetFile "project_discussion_wrapper")\n \n-data UpdateProject = UpdateProject { updateProjectName :: Text, updateProjectDescription :: Markdown, updateProjectTags :: [Text], updateProjectGithubRepo :: Maybe Text } deriving Show\n+--------------------------------------------------------------------------------\n+-- /c/#CommentId/close\n+\n+getCloseProjectCommentR :: Text -> CommentId -> Handler Html\n+getCloseProjectCommentR project_handle comment_id = do\n+    (widget, _) <-\n+        makeProjectCommentActionWidget\n+          makeCloseCommentWidget\n+          project_handle\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $(widgetFile "project_discussion_wrapper")\n+\n+postCloseProjectCommentR :: Text -> CommentId -> Handler Html\n+postCloseProjectCommentR project_handle comment_id = do\n+    (user@(Entity user_id _), (Entity project_id _), comment) <- checkCommentRequireAuth project_handle comment_id\n+    checkProjectCommentActionPermission can_close project_handle (Entity comment_id comment)\n+\n+    postCloseComment\n+      user\n+      comment_id\n+      comment\n+      (projectCommentHandlerInfo (Just user_id) project_id project_handle)\n+      >>= \\case\n+        Nothing -> redirect (ProjectCommentR project_handle comment_id)\n+        Just (widget, form) -> defaultLayout $ previewWidget form "close" ($(widgetFile "project_discussion_wrapper"))\n \n+--------------------------------------------------------------------------------\n+-- /c/#CommentId/delete\n+\n+getDeleteProjectCommentR :: Text -> CommentId -> Handler Html\n+getDeleteProjectCommentR project_handle comment_id = do\n+    (widget, _) <-\n+        makeProjectCommentActionWidget\n+          makeDeleteCommentWidget\n+          project_handle\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $(widgetFile "project_discussion_wrapper")\n+\n+postDeleteProjectCommentR :: Text -> CommentId -> Handler Html\n+postDeleteProjectCommentR project_handle comment_id = do\n+    (_, _, comment) <- checkComment project_handle comment_id\n+    checkProjectCommentActionPermission can_delete project_handle (Entity comment_id comment)\n+\n+    was_deleted <- postDeleteComment comment_id\n+    if was_deleted\n+        then redirect (ProjectDiscussionR project_handle)\n+        else redirect (ProjectCommentR project_handle comment_id)\n \n-editProjectForm :: Maybe (Project, [Text]) -> Form UpdateProject\n-editProjectForm project =\n-    renderBootstrap3 $ UpdateProject\n-        <$> areq' textField "Project Name" (projectName . fst <$> project)\n-        <*> areq' snowdriftMarkdownField "Description" (projectDescription . fst <$> project)\n-        <*> (maybe [] (map T.strip . T.splitOn ",") <$> aopt' textField "Tags" (Just . T.intercalate ", " . snd <$> project))\n-        <*> aopt' textField "Github Repository" (projectGithubRepo . fst <$> project)\n+--------------------------------------------------------------------------------\n+-- /c/#CommentId/edit\n+\n+getEditProjectCommentR :: Text -> CommentId -> Handler Html\n+getEditProjectCommentR project_handle comment_id = do\n+    (widget, _) <-\n+        makeProjectCommentActionWidget\n+          makeEditCommentWidget\n+          project_handle\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $(widgetFile "project_discussion_wrapper")\n+\n+postEditProjectCommentR :: Text -> CommentId -> Handler Html\n+postEditProjectCommentR project_handle comment_id = do\n+    (user@(Entity user_id _), Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id\n+    checkProjectCommentActionPermission can_edit project_handle (Entity comment_id comment)\n+\n+    postEditComment\n+      user\n+      (Entity comment_id comment)\n+      (projectCommentHandlerInfo (Just user_id) project_id project_handle)\n+      >>= \\case\n+        Nothing -> redirect (ProjectCommentR project_handle comment_id)         -- Edit made.\n+        Just widget -> defaultLayout $(widgetFile "project_discussion_wrapper") -- Previewing edit.\n \n+--------------------------------------------------------------------------------\n+-- /c/#CommentId/flag\n+\n+getFlagProjectCommentR :: Text -> CommentId -> Handler Html\n+getFlagProjectCommentR project_handle comment_id = do\n+    (widget, _) <-\n+        makeProjectCommentActionWidget\n+          makeFlagCommentWidget\n+          project_handle\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $(widgetFile "project_discussion_wrapper")\n+\n+postFlagProjectCommentR :: Text -> CommentId -> Handler Html\n+postFlagProjectCommentR project_handle comment_id = do\n+    (user@(Entity user_id _), Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id\n+    checkProjectCommentActionPermission can_flag project_handle (Entity comment_id comment)\n+\n+    postFlagComment\n+      user\n+      (Entity comment_id comment)\n+      (projectCommentHandlerInfo (Just user_id) project_id project_handle)\n+      >>= \\case\n+        Nothing -> redirect (ProjectDiscussionR project_handle)\n+        Just widget -> defaultLayout $(widgetFile "project_discussion_wrapper")\n+\n+--------------------------------------------------------------------------------\n+-- /moderate TODO: rename to /approve\n+\n+getApproveProjectCommentR :: Text -> CommentId -> Handler Html\n+getApproveProjectCommentR project_handle comment_id = do\n+    (widget, _) <-\n+        makeProjectCommentActionWidget\n+          makeApproveCommentWidget\n+          project_handle\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $(widgetFile "project_discussion_wrapper")\n+\n+postApproveProjectCommentR :: Text -> CommentId -> Handler Html\n+postApproveProjectCommentR project_handle comment_id = do\n+    (Entity user_id _, _, comment) <- checkCommentRequireAuth project_handle comment_id\n+    checkProjectCommentActionPermission can_approve project_handle (Entity comment_id comment)\n+\n+    postApproveComment user_id comment_id comment\n+    redirect (ProjectCommentR project_handle comment_id)\n+\n+--------------------------------------------------------------------------------\n+-- /c/#CommentId/reply\n+\n+getReplyProjectCommentR :: Text -> CommentId -> Handler Html\n+getReplyProjectCommentR project_handle parent_id = do\n+    (widget, _) <-\n+        makeProjectCommentActionWidget\n+          makeReplyCommentWidget\n+          project_handle\n+          parent_id\n+          def\n+          getMaxDepth\n+    defaultLayout $(widgetFile "project_discussion_wrapper")\n+\n+postReplyProjectCommentR :: Text -> CommentId -> Handler Html\n+postReplyProjectCommentR project_handle parent_id = do\n+    (user, Entity _ project, parent) <- checkCommentRequireAuth project_handle parent_id\n+    checkProjectCommentActionPermission can_reply project_handle (Entity parent_id parent)\n+\n+    postNewComment\n+      (Just parent_id)\n+      user\n+      (projectDiscussion project)\n+      (makeProjectCommentActionPermissions project_handle) >>= \\case\n+        Left _ -> redirect (ProjectCommentR project_handle parent_id)\n+        Right (widget, form) -> defaultLayout $ previewWidget form "post" ($(widgetFile "project_discussion_wrapper"))\n+\n+--------------------------------------------------------------------------------\n+-- /c/#CommentId/rethread\n+\n+getRethreadProjectCommentR :: Text -> CommentId -> Handler Html\n+getRethreadProjectCommentR project_handle comment_id = do\n+    (widget, _) <-\n+        makeProjectCommentActionWidget\n+          makeRethreadCommentWidget\n+          project_handle\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $(widgetFile "project_discussion_wrapper")\n+\n+postRethreadProjectCommentR :: Text -> CommentId -> Handler Html\n+postRethreadProjectCommentR project_handle comment_id = do\n+    (Entity user_id _, _, comment) <- checkCommentRequireAuth project_handle comment_id\n+    checkProjectCommentActionPermission can_rethread project_handle (Entity comment_id comment)\n+    postRethreadComment user_id comment_id comment\n+\n+--------------------------------------------------------------------------------\n+-- /c/#CommentId/retract\n+\n+getRetractProjectCommentR :: Text -> CommentId -> Handler Html\n+getRetractProjectCommentR project_handle comment_id = do\n+    (widget, _) <-\n+        makeProjectCommentActionWidget\n+          makeRetractCommentWidget\n+          project_handle\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $(widgetFile "project_discussion_wrapper")\n+\n+postRetractProjectCommentR :: Text -> CommentId -> Handler Html\n+postRetractProjectCommentR project_handle comment_id = do\n+    (user@(Entity user_id _), Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id\n+    checkProjectCommentActionPermission can_retract project_handle (Entity comment_id comment)\n+\n+    postRetractComment\n+      user\n+      comment_id\n+      comment\n+      (projectCommentHandlerInfo (Just user_id) project_id project_handle)\n+      >>= \\case\n+        Nothing -> redirect (ProjectCommentR project_handle comment_id)\n+        Just (widget, form) -> defaultLayout $ previewWidget form "retract" ($(widgetFile "project_discussion_wrapper"))\n+\n+--------------------------------------------------------------------------------\n+-- /c/#CommentId/tags\n+\n+getProjectCommentTagsR :: Text -> CommentId -> Handler Html\n+getProjectCommentTagsR _ = getCommentTags\n+\n+--------------------------------------------------------------------------------\n+-- /c/#CommentId/tag/#TagId\n+\n+getProjectCommentTagR :: Text -> CommentId -> TagId -> Handler Html\n+getProjectCommentTagR _ = getCommentTagR\n+\n+postProjectCommentTagR :: Text -> CommentId -> TagId -> Handler Html\n+postProjectCommentTagR project_handle comment_id tag_id = do\n+    postCommentTag comment_id tag_id\n+    redirect (ProjectCommentTagR project_handle comment_id tag_id)\n+\n+--------------------------------------------------------------------------------\n+-- /c/#CommentId/tag/apply, /c/#CommentId/tag/create\n+\n+postProjectCommentApplyTagR, postProjectCommentCreateTagR:: Text -> CommentId -> Handler Html\n+postProjectCommentApplyTagR  = applyOrCreate postCommentApplyTag\n+postProjectCommentCreateTagR = applyOrCreate postCommentCreateTag\n+\n+applyOrCreate :: (CommentId -> Handler ()) -> Text -> CommentId -> Handler Html\n+applyOrCreate action project_handle comment_id = do\n+    action comment_id\n+    redirect (ProjectCommentR project_handle comment_id)\n+\n+--------------------------------------------------------------------------------\n+-- /c/#CommentId/tag/new\n+\n+getProjectCommentAddTagR :: Text -> CommentId -> Handler Html\n+getProjectCommentAddTagR project_handle comment_id = do\n+    (Entity user_id _, Entity project_id _, comment) <- checkCommentRequireAuth project_handle comment_id\n+    ok <- can_add_tag <$> makeProjectCommentActionPermissions project_handle (Entity comment_id comment)\n+    unless ok (permissionDenied "You don't have permission to view this page.")\n+    getProjectCommentAddTag comment_id project_id user_id\n+\n+--------------------------------------------------------------------------------\n+-- /d\n+\n+getProjectDiscussionR :: Text -> Handler Html\n+getProjectDiscussionR = getDiscussion . getProjectDiscussion\n+\n+getProjectDiscussion :: Text -> (DiscussionId -> ExprCommentCond -> DB [Entity Comment]) -> Handler Html\n+getProjectDiscussion project_handle get_root_comments = do\n+    muser <- maybeAuth\n+    let muser_id = entityKey <$> muser\n+\n+    (Entity project_id project, root_comments) <- runYDB $ do\n+        p@(Entity project_id project) <- getBy404 (UniqueProjectHandle project_handle)\n+        let has_permission = exprCommentProjectPermissionFilter muser_id (val project_id)\n+        root_comments <- get_root_comments (projectDiscussion project) has_permission\n+        return (p, root_comments)\n+\n+    (comment_forest_no_css, _) <-\n+        makeProjectCommentForestWidget\n+          muser\n+          project_id\n+          project_handle\n+          root_comments\n+          def\n+          (getMaxDepthDefault 0)\n+          False\n+          mempty\n+\n+    let has_comments = not (null root_comments)\n+        comment_forest = do\n+            comment_forest_no_css\n+            toWidget $(cassiusFile "templates/comment.cassius")\n+\n+    (comment_form, _) <- generateFormPost commentNewTopicForm\n+\n+    defaultLayout $ do\n+        setTitle . toHtml $ projectName project <> " Discussion | Snowdrift.coop"\n+        $(widgetFile "project_discuss")\n+\n+--------------------------------------------------------------------------------\n+-- /d/new\n+\n+getNewProjectDiscussionR :: Text -> Handler Html\n+getNewProjectDiscussionR project_handle = do\n+    void requireAuth\n+    let widget = commentNewTopicFormWidget\n+    defaultLayout $(widgetFile "project_discussion_wrapper")\n+\n+postNewProjectDiscussionR :: Text -> Handler Html\n+postNewProjectDiscussionR project_handle = do\n+    user <- requireAuth\n+    Entity _ Project{..} <- runYDB (getBy404 (UniqueProjectHandle project_handle))\n+\n+    postNewComment\n+      Nothing\n+      user\n+      projectDiscussion\n+      (makeProjectCommentActionPermissions project_handle) >>= \\case\n+        Left comment_id -> redirect (ProjectCommentR project_handle comment_id)\n+        Right (widget, form) -> defaultLayout $ previewWidget form "post" ($(widgetFile "project_discussion_wrapper"))\n+\n+--------------------------------------------------------------------------------\n+-- /edit\n \n getEditProjectR :: Text -> Handler Html\n getEditProjectR project_handle = do\n-    viewer_id <- requireAuthId\n-\n-    Entity project_id project <- runYDB $ do\n-        can_edit <- (||) <$> isProjectAdmin project_handle viewer_id <*> isProjectAdmin "snowdrift" viewer_id\n-        if can_edit\n-         then getBy404 $ UniqueProjectHandle project_handle\n-         else permissionDenied "You do not have permission to edit this project."\n+    (_, Entity project_id project) <-\n+        requireRolesAny [Admin] project_handle "You do not have permission to edit this project."\n \n     tags <- runDB $\n         select $\n@@ -153,69 +751,170 @@ getEditProjectR project_handle = do\n         setTitle . toHtml $ projectName project <> " | Snowdrift.coop"\n         $(widgetFile "edit_project")\n \n+--------------------------------------------------------------------------------\n+-- /feed\n \n-postProjectR :: Text -> Handler Html\n-postProjectR project_handle = do\n-    viewer_id <- requireAuthId\n+-- | This function is responsible for hitting every relevant event table. Nothing\n+-- statically guarantees that.\n+getProjectFeedR :: Text -> Handler Html\n+getProjectFeedR project_handle = do\n+    let lim = 26 -- limit n from each table, then take (n-1)\n+\n+    muser_id <- maybeAuthId\n+    before <- lookupGetUTCTimeDefaultNow "before"\n+    (project, all_unsorted_events, discussion_wiki_page_map,\n+      wiki_page_map, user_map, earlier_closures_map, closure_map,\n+      ticket_map, flag_map) <- runYDB $ do\n+        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)\n+\n+        project_comments   <- fetchProjectCommentsBeforeDB         project_id muser_id before lim\n+        wiki_page_comments <- fetchProjectWikiPageCommentsBeforeDB project_id muser_id before lim\n+        wiki_pages         <- fetchProjectWikiPagesBeforeDB        project_id before lim\n+        wiki_edit_entities <- fetchProjectWikiEditsBeforeDB        project_id before lim\n+        new_pledges        <- fetchProjectNewPledgesBeforeDB       project_id before lim\n+        updated_pledges    <- fetchProjectUpdatedPledgesBeforeDB   project_id before lim\n+        deleted_pledges    <- fetchProjectDeletedPledgesBeforeDB   project_id before lim\n+\n+        -- Suplementary maps for displaying the data. If something above requires extra\n+        -- data to display the project feed row, it MUST be used to fetch the data below!\n+        -- The Maybes from Data.Map.lookup are unsafely STRIPPED in the views!\n+\n+        let comment_entities = project_comments <> wiki_page_comments\n+            comment_ids      = map entityKey comment_entities\n+            comments         = map entityVal comment_entities\n+            wiki_edits       = map entityVal wiki_edit_entities\n+            shares_pledged   = map entityVal (new_pledges <> (map snd updated_pledges))\n+            -- All users: Comment posters, WikiPage creators, WikiEdit makers,\n+            -- and Pledgers (new, updated, and deleted).\n+            user_ids = S.toList $\n+                         S.fromList (map commentUser comments) <>\n+                         S.fromList (map wikiEditUser wiki_edits) <>\n+                         S.fromList (map sharesPledgedUser shares_pledged) <>\n+                         S.fromList (map eventDeletedPledgeUser deleted_pledges)\n+\n+        -- WikiPages that can be keyed by a Comment's DiscussionId (i.e. the Comment *is* on a WikiPage)\n+        discussion_wiki_page_map <- M.fromList . map (\\e@(Entity _ WikiPage{..}) -> (wikiPageDiscussion, e)) <$>\n+                                       fetchDiscussionWikiPagesInDB (map commentDiscussion comments)\n \n-    Entity project_id project <- runYDB $ do\n-        can_edit <- (||) <$> isProjectAdmin project_handle viewer_id <*> isProjectAdmin "snowdrift" viewer_id\n-        if can_edit\n-         then getBy404 $ UniqueProjectHandle project_handle\n-         else permissionDenied "You do not have permission to edit this project."\n+        -- WikiPages keyed by their own IDs (contained in a WikiEdit)\n+        wiki_page_map <- entitiesMap <$> fetchWikiPagesInDB (map wikiEditPage wiki_edits)\n \n-    ((result, _), _) <- runFormPost $ editProjectForm Nothing\n+        user_map <- entitiesMap <$> fetchUsersInDB user_ids\n+\n+        earlier_closures_map <- fetchCommentsAncestorClosuresDB comment_ids\n+        closure_map          <- makeClosureMapDB comment_ids\n+        ticket_map           <- makeTicketMapDB  comment_ids\n+        flag_map             <- makeFlagMapDB    comment_ids\n+\n+        let all_unsorted_events = mconcat\n+              [ map (onEntity ECommentPosted)  comment_entities\n+              , map (onEntity EWikiPage)       wiki_pages\n+              , map (onEntity EWikiEdit)       wiki_edit_entities\n+              , map (onEntity ENewPledge)      new_pledges\n+              , map eup2se                     updated_pledges\n+              , map edp2se                     deleted_pledges\n+              ]\n+\n+        return (project, all_unsorted_events, discussion_wiki_page_map,\n+          wiki_page_map, user_map, earlier_closures_map, closure_map,\n+          ticket_map, flag_map)\n+\n+    let (events, more_events) = splitAt (lim-1) (sortBy snowdriftEventNewestToOldest all_unsorted_events)\n+\n+        -- For pagination: Nothing means no more pages, Just time means set the 'before'\n+        -- GET param to that time. Note that this means 'before' should be a <= relation,\n+        -- rather than a <.\n+        mnext_before :: Maybe Text\n+        mnext_before = case more_events of\n+          []             -> Nothing\n+          (next_event:_) -> (Just . T.pack . show . snowdriftEventTime) next_event\n+\n+    defaultLayout $ do\n+        $(widgetFile "project_feed")\n+        toWidget $(cassiusFile "templates/comment.cassius")\n+  where\n+    -- "event updated pledge to snowdrift event". Makes above code cleaner.\n+    eup2se :: (Int64, Entity SharesPledged) -> SnowdriftEvent\n+    eup2se (old_shares, Entity shares_pledged_id shares_pledged) = EUpdatedPledge old_shares shares_pledged_id shares_pledged\n+\n+    -- "event deleted pledge to snowdrift event". Makes above code cleaner.\n+    edp2se :: EventDeletedPledge -> SnowdriftEvent\n+    edp2se (EventDeletedPledge a b c d) = EDeletedPledge a b c d\n+\n+--------------------------------------------------------------------------------\n+-- /invite\n+\n+getInviteR :: Text -> Handler Html\n+getInviteR project_handle = do\n+    (_, Entity _ project) <- requireRolesAny [Admin] project_handle "You must be a project admin to invite."\n \n     now <- liftIO getCurrentTime\n+    maybe_invite_code <- lookupSession "InviteCode"\n+    maybe_invite_role <- fmap (read . T.unpack) <$> lookupSession "InviteRole"\n+    deleteSession "InviteCode"\n+    deleteSession "InviteRole"\n+    let maybe_link = InvitationR project_handle <$> maybe_invite_code\n+    (invite_form, _) <- generateFormPost inviteForm\n+\n+    outstanding_invites <- runDB $\n+        select $\n+        from $ \\ invite -> do\n+        where_ ( invite ^. InviteRedeemed ==. val False )\n+        orderBy [ desc (invite ^. InviteCreatedTs) ]\n+        return invite\n \n-    case result of\n-        FormSuccess (UpdateProject name description tags github_repo) -> do\n-            mode <- lookupPostParam "mode"\n-            let action :: Text = "update"\n-            case mode of\n-                Just "preview" -> do\n-                    let preview_project = project { projectName = name, projectDescription = description, projectGithubRepo = github_repo }\n+    redeemed_invites <- runDB $\n+        select $\n+        from $ \\ invite -> do\n+        where_ ( invite ^. InviteRedeemed ==. val True )\n+        orderBy [ desc (invite ^. InviteCreatedTs) ]\n+        limit 20\n+        return invite\n \n-                    (form, _) <- generateFormPost $ editProjectForm (Just (preview_project, tags))\n-                    defaultLayout $ previewWidget form action $ renderProject (Just project_id) preview_project [] Nothing\n+    let redeemed_users = S.fromList $ mapMaybe (inviteRedeemedBy . entityVal) redeemed_invites\n+        redeemed_inviters = S.fromList $ map (inviteUser . entityVal) redeemed_invites\n+        outstanding_inviters = S.fromList $ map (inviteUser . entityVal) outstanding_invites\n+        user_ids = S.toList $ redeemed_users `S.union` redeemed_inviters `S.union` outstanding_inviters\n \n-                Just x | x == action -> do\n-                    runDB $ do\n-                        when (projectDescription project /= description) $ do\n-                            project_update <- insert $ ProjectUpdate now project_id viewer_id $ diffMarkdown (projectDescription project) description\n-                            last_update <- getBy $ UniqueProjectLastUpdate project_id\n-                            case last_update of\n-                                Just (Entity key _) -> repsert key $ ProjectLastUpdate project_id project_update\n-                                Nothing -> void $ insert $ ProjectLastUpdate project_id project_update\n+    user_entities <- runDB $ selectList [ UserId <-. user_ids ] []\n \n-                        update $ \\ p -> do\n-                            set p [ ProjectName =. val name, ProjectDescription =. val description, ProjectGithubRepo =. val github_repo ]\n-                            where_ (p ^. ProjectId ==. val project_id)\n+    let users = M.fromList $ map (entityKey &&& id) user_entities\n \n-                        tag_ids <- forM tags $ \\ tag_name -> do\n-                            tag_entity_list <- select $ from $ \\ tag -> do\n-                                where_ (tag ^. TagName ==. val tag_name)\n-                                return tag\n+    let format_user Nothing = "NULL"\n+        format_user (Just user_id) =\n+            let Entity _ user = fromMaybe (error "getInviteR: user_id not found in users map")\n+                                          (M.lookup user_id users)\n+             in fromMaybe (userIdent user) $ userName user\n \n-                            case tag_entity_list of\n-                                [] -> insert $ Tag tag_name\n-                                Entity tag_id _ : _ -> return tag_id\n+        format_inviter user_id =\n+            userDisplayName $ fromMaybe (error "getInviteR(#2): user_id not found in users map")\n+                                        (M.lookup user_id users)\n \n+    defaultLayout $ do\n+        setTitle . toHtml $ projectName project <> " - Send Invite | Snowdrift.coop"\n+        $(widgetFile "invite")\n \n-                        delete $ from $ \\ project_tag -> where_ (project_tag ^. ProjectTagProject ==. val project_id)\n+postInviteR :: Text -> Handler Html\n+postInviteR project_handle = do\n+    (user_id, Entity project_id _) <- requireRolesAny [Admin] project_handle "You must be a project admin to invite."\n \n-                        forM_ tag_ids $ \\ tag_id -> insert $ ProjectTag project_id tag_id\n+    now <- liftIO getCurrentTime\n+    invite <- liftIO randomIO\n \n-                    addAlert "success" "project updated"\n-                    redirect $ ProjectR project_handle\n+    ((result, _), _) <- runFormPost inviteForm\n+    case result of\n+        FormSuccess (tag, role) -> do\n+            let invite_code = T.pack $ printf "%016x" (invite :: Int64)\n+            _ <- runDB $ insert $ Invite now project_id invite_code user_id role tag False Nothing Nothing\n+            setSession "InviteCode" invite_code\n+            setSession "InviteRole" (T.pack $ show role)\n \n-                _ -> do\n-                    addAlertEm "danger" "unrecognized mode" "Error: "\n-                    redirect $ ProjectR project_handle\n-        x -> do\n-            addAlert "danger" $ T.pack $ show x\n-            redirect $ ProjectR project_handle\n+        _ -> alertDanger "Error in submitting form."\n \n+    redirect $ InviteR project_handle\n+\n+--------------------------------------------------------------------------------\n+-- /patrons\n \n getProjectPatronsR :: Text -> Handler Html\n getProjectPatronsR project_handle = do\n@@ -255,6 +954,35 @@ getProjectPatronsR project_handle = do\n         setTitle . toHtml $ projectName project <> " Patrons | Snowdrift.coop"\n         $(widgetFile "project_patrons")\n \n+--------------------------------------------------------------------------------\n+-- /t\n+\n+getTicketsR :: Text -> Handler Html\n+getTicketsR project_handle = do\n+    muser_id <- maybeAuthId\n+    (project, tagged_tickets) <- runYDB $ do\n+        Entity project_id project <- getBy404 (UniqueProjectHandle project_handle)\n+        tagged_tickets <- fetchProjectTaggedTicketsDB project_id muser_id\n+        return (project, tagged_tickets)\n+\n+    ((result, formWidget), encType) <- runFormGet viewForm\n+    let (filter_expression, order_expression) = case result of\n+            FormSuccess x -> x\n+            _ -> (defaultFilter, defaultOrder)\n+\n+    github_issues <- getGithubIssues project\n+\n+    let issues = sortBy (flip compare `on` order_expression . issueOrderable) $\n+                   filter (filter_expression . issueFilterable) $\n+                      map mkSomeIssue tagged_tickets ++ map mkSomeIssue github_issues\n+\n+    defaultLayout $ do\n+        setTitle . toHtml $ projectName project <> " Tickets | Snowdrift.coop"\n+        $(widgetFile "tickets")\n+\n+--------------------------------------------------------------------------------\n+-- /transactions\n+\n getProjectTransactionsR :: Text -> Handler Html\n getProjectTransactionsR project_handle = do\n     (project, account, account_map, transaction_groups) <- runYDB $ do\n@@ -309,108 +1037,34 @@ getProjectTransactionsR project_handle = do\n                 = (fmap (payday_map M.!) $ transactionPayday $ entityVal t', reverse (t':ts')) : process' [t] ts\n          in process' []\n \n+--------------------------------------------------------------------------------\n+-- /w\n \n-getProjectBlogR :: Text -> Handler Html\n-getProjectBlogR project_handle = do\n-    maybe_from <- fmap (Key . PersistInt64 . read . T.unpack) <$> lookupGetParam "from"\n-    post_count <- fromMaybe 10 <$> fmap (read . T.unpack) <$> lookupGetParam "from"\n-    Entity project_id project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle\n-\n-    let apply_offset blog = maybe id (\\ from_blog rest -> blog ^. ProjectBlogId >=. val from_blog &&. rest) maybe_from\n-\n-    (posts, next) <- fmap (splitAt post_count) $ runDB $\n-        select $\n-        from $ \\blog -> do\n-        where_ $ apply_offset blog $ blog ^. ProjectBlogProject ==. val project_id\n-        orderBy [ desc $ blog ^. ProjectBlogTime, desc $ blog ^. ProjectBlogId ]\n-        limit (fromIntegral post_count + 1)\n-        return blog\n-\n-    renderRouteParams <- getUrlRenderParams\n-\n-    let nextRoute next_id = renderRouteParams (ProjectBlogR project_handle) [("from", toPathPiece next_id)]\n-\n-    defaultLayout $ do\n-        setTitle . toHtml $ projectName project <> " Blog | Snowdrift.coop"\n-        $(widgetFile "project_blog")\n-\n-projectBlogForm :: UTCTime -> UserId -> ProjectId -> Form ProjectBlog\n-projectBlogForm now user_id project_id =\n-    renderBootstrap3 $ mkBlog\n-        <$> areq' textField "Post Title" Nothing\n-        <*> areq' snowdriftMarkdownField "Post" Nothing\n-  where\n-    mkBlog :: Text -> Markdown -> ProjectBlog\n-    mkBlog title (Markdown content) =\n-        let (top_content, bottom_content) = break (== "---") $ T.lines content\n-         in ProjectBlog now title user_id project_id undefined (Markdown $ T.unlines top_content) (if null bottom_content then Nothing else Just $ Markdown $ T.unlines bottom_content)\n-\n-\n-postProjectBlogR :: Text -> Handler Html\n-postProjectBlogR project_handle = do\n-    viewer_id <- requireAuthId\n-\n-    Entity project_id _ <- runYDB $ do\n-        can_edit <- or <$> sequence\n-            [ isProjectAdmin project_handle viewer_id\n-            , isProjectTeamMember project_handle viewer_id\n-            , isProjectAdmin "snowdrift" viewer_id\n-            ]\n-\n-        if can_edit\n-         then getBy404 $ UniqueProjectHandle project_handle\n-         else permissionDenied "You do not have permission to edit this project."\n-\n-    now <- liftIO getCurrentTime\n-\n-    ((result, _), _) <- runFormPost $ projectBlogForm now viewer_id project_id\n-\n-    case result of\n-        FormSuccess blog_post' -> do\n-            let blog_post :: ProjectBlog\n-                blog_post = blog_post' { projectBlogTime = now, projectBlogUser = viewer_id }\n-            mode <- lookupPostParam "mode"\n-            let action :: Text = "post"\n-            case mode of\n-                Just "preview" -> do\n-\n-                    (form, _) <- generateFormPost $ projectBlogForm now viewer_id project_id\n-\n-                    defaultLayout $ previewWidget form action $ renderBlogPost project_handle blog_post\n-\n-                Just x | x == action -> do\n-                    void $ runDB $ insert blog_post\n-                    addAlert "success" "posted"\n-                    redirect $ ProjectR project_handle\n-\n-                _ -> do\n-                    addAlertEm "danger" "unrecognized mode" "Error: "\n-                    redirect $ ProjectR project_handle\n-\n-        x -> do\n-            addAlert "danger" $ T.pack $ show x\n-            redirect $ ProjectR project_handle\n-\n-\n-getProjectBlogPostR :: Text -> ProjectBlogId -> Handler Html\n-getProjectBlogPostR project_handle blog_post_id = do\n-    (Entity _ project, blog_post) <- runYDB $ (,)\n-        <$> getBy404 (UniqueProjectHandle project_handle)\n-        <*> get404 blog_post_id\n-\n+getWikiPagesR :: Text -> Handler Html\n+getWikiPagesR project_handle = do\n+    muser_id <- maybeAuthId\n+    -- TODO: should be be using unviewed_comments and unviewed_edits?\n+    (project, pages, _, _) <- runYDB $ do\n+        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle\n+        pages <- getProjectWikiPages project_id\n+\n+        -- If the user is not logged in or not watching the project, these maps are empty.\n+        (unviewed_comments, unviewed_edits) <- case muser_id of\n+            Nothing -> return (mempty, mempty)\n+            Just user_id -> do\n+                is_watching <- userIsWatchingProjectDB user_id project_id\n+                if is_watching\n+                    then (,)\n+                        <$> fetchNumUnviewedCommentsOnProjectWikiPagesDB user_id project_id\n+                        <*> fetchNumUnviewedWikiEditsOnProjectDB         user_id project_id\n+                    else return (mempty, mempty)\n+        return (project, pages, unviewed_comments, unviewed_edits)\n     defaultLayout $ do\n-        setTitle . toHtml $ projectName project <> " Blog - " <> projectBlogTitle blog_post <> " | Snowdrift.coop"\n-        renderBlogPost project_handle blog_post\n+        setTitle . toHtml $ projectName project <> " Wiki | Snowdrift.coop"\n+        $(widgetFile "wiki_pages")\n \n-\n-renderBlogPost :: Text -> ProjectBlog -> WidgetT App IO ()\n-renderBlogPost project_handle blog_post = do\n-    let (Markdown top_content) = projectBlogTopContent blog_post\n-        (Markdown bottom_content) = fromMaybe (Markdown "") $ projectBlogBottomContent blog_post\n-        title = projectBlogTitle blog_post\n-        content = markdownWidget project_handle $ Markdown $ T.snoc top_content '\\n' <> bottom_content\n-\n-    $(widgetFile "blog_post")\n+--------------------------------------------------------------------------------\n+-- /watch, /unwatch\n \n postWatchProjectR, postUnwatchProjectR :: ProjectId -> Handler ()\n postWatchProjectR   = watchOrUnwatchProject userWatchProjectDB   "watching "\n@@ -422,39 +1076,5 @@ watchOrUnwatchProject action msg project_id = do\n     project <- runYDB $ do\n         action user_id project_id\n         get404 project_id\n-    addAlert "success" (msg <> projectName project)\n+    alertSuccess (msg <> projectName project)\n     redirect HomeR\n-\n---------------------------------------------------------------------------------\n--- /feed\n-\n--- | This function is responsible for hitting every relevant event table. Nothing\n--- statically guarantees that.\n-getProjectFeedR :: Text -> Handler Html\n-getProjectFeedR project_handle = do\n-    before <- maybe (liftIO getCurrentTime) (return . read . T.unpack) =<< lookupGetParam "before"\n-    (events, discussion_wiki_pages_map, wiki_pages_map, _) <- runYDB $ do\n-        Entity project_id _ <- getBy404 (UniqueProjectHandle project_handle)\n-        comment_entities   <- fetchProjectCommentsPostedOnWikiPagesBeforeDB project_id before\n-        wiki_edit_entities <- fetchProjectWikiEditsBeforeDB                 project_id before\n-\n-        -- Suplementary maps for displaying the data.\n-\n-        let comments   = map entityVal comment_entities\n-            wiki_edits = map entityVal wiki_edit_entities\n-\n-        discussion_wiki_pages_map <- M.fromList . map (\\e@(Entity _ WikiPage{..}) -> (wikiPageDiscussion, e)) <$>\n-                                       fetchDiscussionWikiPagesInDB (map commentDiscussion comments)\n-\n-        wiki_pages_map <- entitiesMap <$> fetchWikiPagesInDB (map wikiEditPage wiki_edits)\n-\n-        users_map <- (<>)\n-            <$> (entitiesMap <$> fetchUsersInDB (map commentUser comments))\n-            <*> (entitiesMap <$> fetchUsersInDB (map wikiEditUser wiki_edits))\n-\n-        let events = sortBy snowdriftEventNewestToOldest . mconcat $\n-              [ map (onEntity ECommentPosted) comment_entities\n-              , map (onEntity EWikiEdit)      wiki_edit_entities\n-              ]\n-        return (events, discussion_wiki_pages_map, wiki_pages_map, users_map)\n-    defaultLayout $(widgetFile "project_feed")\ndiff --git a/Handler/Tickets.hs b/Handler/Tickets.hs\ndeleted file mode 100644\nindex 6dfb956..0000000\n--- a/Handler/Tickets.hs\n+++ /dev/null\n@@ -1,40 +0,0 @@\n-{-# LANGUAGE TupleSections #-}\n-\n-module Handler.Tickets where\n-\n-import Import\n-\n-import           Data.Filter\n-import           Data.Order\n-import           Model.Issue\n-import           Model.Project            (getGithubIssues)\n-import           Model.Ticket             (getTickets)\n-\n-import           Data.List                (sortBy)\n-\n-viewForm :: Form (Filterable -> Bool, Orderable -> [Double])\n-viewForm = renderBootstrap3 $ (,)\n-    <$> (either (const defaultFilter) id . parseFilterExpression . fromMaybe "" <$> aopt' textField "filter" Nothing)\n-    <*> (either (const defaultOrder) id . parseOrderExpression . fromMaybe "" <$> aopt' textField "sort" Nothing)\n-\n-\n-getTicketsR :: Text -> Handler Html\n-getTicketsR project_handle = do\n-    Entity project_id project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle\n-\n-    ((result, formWidget), encType) <- runFormGet viewForm\n-\n-    let (filter_expression, order_expression) = case result of\n-            FormSuccess x -> x\n-            _ -> (defaultFilter, defaultOrder)\n-\n-    tickets       <- runYDB $ getTickets project_id project_handle\n-    github_issues <- getGithubIssues project\n-\n-    let issues = sortBy (flip compare `on` order_expression . issueOrderable) $\n-                   filter (filter_expression . issueFilterable) $\n-                      map mkSomeIssue tickets ++ map mkSomeIssue github_issues\n-\n-    defaultLayout $ do\n-        setTitle . toHtml $ projectName project <> " Tickets | Snowdrift.coop"\n-        $(widgetFile "tickets")\ndiff --git a/Handler/UpdateShares.hs b/Handler/UpdateShares.hs\nindex 225daf6..b4d137d 100644\n--- a/Handler/UpdateShares.hs\n+++ b/Handler/UpdateShares.hs\n@@ -36,56 +36,32 @@ getUpdateSharesR project_handle = do\n \n postUpdateSharesR :: Text -> Handler Html\n postUpdateSharesR project_handle = do\n-    user_id <- requireAuthId\n     ((result, _), _) <- runFormPost $ confirmForm 1\n-    now <- liftIO getCurrentTime\n \n     case result of\n         FormSuccess (SharesPurchaseOrder shares) -> do\n             -- TODO - refuse negative\n             Just pledge_render_id <- fmap (read . T.unpack) <$> lookupSession pledgeRenderKey\n \n-            success <- runYDB $ do\n-                Just user <- get user_id\n-                Just account <- get $ userAccount user\n-                Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle\n-                _ <- insert $ SharesPledged now user_id shares pledge_render_id\n-\n-                either_unique <- insertBy $ Pledge user_id project_id shares shares\n-\n-                case either_unique of\n-                    Right _ -> return ()\n-                    Left (Entity pledge_id _) ->\n-                        if shares == 0\n-                         then delete $ from $ \\ pledge -> where_ (pledge ^. PledgeId ==. val pledge_id)\n-                         else update $ \\ pledge -> do\n-                            set pledge [ PledgeShares =. val shares\n-                                       , PledgeFundedShares =. val shares\n-                                       ]\n-                            where_ (pledge ^. PledgeId ==. val pledge_id)\n-\n-                updateShareValue project_id\n-\n-                user_pledges <- rawSql\n-                    "SELECT ??, ?? FROM pledge JOIN project ON pledge.project = project.id WHERE pledge.\\"user\\" = ?;" [ unKey user_id ]\n-\n-                let user_outlay = sum $ map (\\ (Entity _ pledge, Entity _ project) ->\n-                                    projectShareValue project $* fromIntegral (pledgeShares pledge)) user_pledges :: Milray\n+            success <- runSYDB $ do\n+                Entity user_id user <- lift (lift requireAuth)\n+                Just account <- lift $ get (userAccount user)\n+                Entity project_id project <- lift $ getBy404 (UniqueProjectHandle project_handle)\n \n+                let user_outlay = projectShareValue project $* fromIntegral shares :: Milray\n                 if accountBalance account < user_outlay $* 3\n-                 then do\n-                    transactionUndo\n-                    return False\n-                 else return True\n+                    then return False\n+                    else do\n+                        insertProjectPledgeDB user_id project_id shares pledge_render_id\n+                        lift (updateShareValue project_id)\n+                        return True\n \n             if success\n-             then addAlert "success" "you are now pledged to support this project"\n-             else addAlert "warning"\n-                "Sorry, you must have funds to support your pledge for at least 3 months at current share value. Please deposit additional funds to your account."\n+               then alertSuccess "you are now pledged to support this project"\n+               else alertWarning "Sorry, you must have funds to support your pledge for at least 3 months at current share value. Please deposit additional funds to your account."\n \n-            redirect $ ProjectR project_handle\n+            redirect (ProjectR project_handle)\n \n         _ -> do\n-            addAlert "danger" "error occurred in form submission"\n-            redirect $ UpdateSharesR project_handle\n-\n+            alertDanger "error occurred in form submission"\n+            redirect (UpdateSharesR project_handle)\ndiff --git a/Handler/User.hs b/Handler/User.hs\nindex c43f7b3..950958e 100644\n--- a/Handler/User.hs\n+++ b/Handler/User.hs\n@@ -3,78 +3,30 @@ module Handler.User where\n import Import\n \n import           Model.Role\n+import           Model.Transaction\n import           Model.User\n import           Widgets.Preview\n import           View.User\n+import           Widgets.ProjectPledges\n+import           Widgets.Time\n \n-import qualified Data.Map as M\n-import qualified Data.Set as S\n+import qualified Data.Map  as M\n+import qualified Data.Set  as S\n import qualified Data.Text as T\n \n-getUserR :: UserId -> Handler Html\n-getUserR user_id = do\n-    mviewer_id <- maybeAuthId\n-\n-    user <- runYDB $ get404 user_id\n-\n-    projects_and_roles <- runDB $ getProjectsAndRoles user_id\n-\n-    defaultLayout $ do\n-        setTitle . toHtml $ "User Profile - " <> userPrintName (Entity user_id user) <> " | Snowdrift.coop"\n-        renderUser mviewer_id user_id user projects_and_roles\n+--------------------------------------------------------------------------------\n+-- Utility functions\n \n-checkEditUser :: UserId -> Handler UserId\n-checkEditUser user_id = do\n-    viewer_id <- requireAuthId\n-    when (user_id /= viewer_id) $ runYDB $ do\n-        is_admin <- isProjectAdmin "snowdrift" viewer_id\n-        unless is_admin $\n-            lift $ permissionDenied "You can only modify your own profile!"\n-    return viewer_id\n-\n-getEditUserR :: UserId -> Handler Html\n-getEditUserR user_id = do\n-    _ <- checkEditUser user_id\n-    user <- runYDB (get404 user_id)\n+lookupParamDefault :: Read a => Text -> a -> Handler a\n+lookupParamDefault name def = do\n+    maybe_param <- lookup name <$> reqGetParams <$> getRequest\n+    return $ fromMaybe def $ do\n+        param_str <- maybe_param\n+        param <- listToMaybe $ reads $ T.unpack param_str\n+        return $ fst param\n \n-    (form, enctype) <- generateFormPost $ editUserForm (Just user)\n-    defaultLayout $ do\n-        setTitle . toHtml $ "User Profile - " <> userPrintName (Entity user_id user) <> " | Snowdrift.coop"\n-        $(widgetFile "edit_user")\n-\n-postUserR :: UserId -> Handler Html\n-postUserR user_id = do\n-    viewer_id <- checkEditUser user_id\n-\n-    ((result, _), _) <- runFormPost $ editUserForm Nothing\n-\n-    case result of\n-        FormSuccess user_update -> do\n-            mode <- lookupPostParam "mode"\n-            let action :: Text = "update"\n-            case mode of\n-                Just "preview" -> do\n-                    user <- runYDB $ get404 user_id\n-\n-                    let updated_user = applyUserUpdate user user_update\n-\n-                    (form, _) <- generateFormPost $ editUserForm (Just updated_user)\n-\n-                    defaultLayout $\n-                        previewWidget form action $\n-                            renderUser (Just viewer_id) user_id updated_user mempty\n-\n-                Just x | x == action -> do\n-                    runDB $ updateUser user_id user_update\n-                    redirect $ UserR user_id\n-\n-                _ -> do\n-                    addAlertEm "danger" "unknown mode" "Error: "\n-                    redirect $ UserR user_id\n-\n-        _ -> do\n-            addAlert "danger" "Failed to update user."\n-            redirect $ UserR user_id\n+--------------------------------------------------------------------------------\n+-- /\n \n getUsersR :: Handler Html\n getUsersR = do\n@@ -107,6 +59,8 @@ getUsersR = do\n         setTitle "Users | Snowdrift.coop"\n         $(widgetFile "users")\n \n+--------------------------------------------------------------------------------\n+-- /new\n \n getUserCreateR :: Handler Html\n getUserCreateR = do\n@@ -129,8 +83,8 @@ postUserCreateR = do\n                 setCreds True $ Creds "HashDB" ident []\n                 redirectUltDest HomeR\n \n-        FormMissing -> addAlert "danger" "missing field"\n-        FormFailure strings -> addAlert "danger" (mconcat strings)\n+        FormMissing -> alertDanger "missing field"\n+        FormFailure strings -> alertDanger (mconcat strings)\n \n     defaultLayout $ [whamlet|\n         <form method=POST>\n@@ -138,7 +92,167 @@ postUserCreateR = do\n             <input type=submit>\n     |]\n \n--- | POST handler for marking a user as eligible for establishment.\n+\n+--------------------------------------------------------------------------------\n+-- /#UserId\n+\n+getUserR :: UserId -> Handler Html\n+getUserR user_id = do\n+    mviewer_id <- maybeAuthId\n+\n+    user <- runYDB $ get404 user_id\n+\n+    projects_and_roles <- runDB (fetchUserProjectsAndRolesDB user_id)\n+\n+    defaultLayout $ do\n+        setTitle . toHtml $ "User Profile - " <> userDisplayName (Entity user_id user) <> " | Snowdrift.coop"\n+        renderUser mviewer_id user_id user projects_and_roles\n+\n+--------------------------------------------------------------------------------\n+-- /#UserId/balance\n+\n+-- check permissions for user balance view\n+getUserBalanceR :: UserId -> Handler Html\n+getUserBalanceR user_id = do\n+    viewer_id <- requireAuthId\n+    if viewer_id /= user_id\n+        then permissionDenied "You must be a site administrator to view user balances."\n+        else getUserBalanceR' user_id\n+\n+getUserBalanceR' :: UserId -> Handler Html\n+getUserBalanceR' user_id = do\n+    user <- runYDB $ get404 user_id\n+\n+    -- TODO: restrict viewing balance to user or snowdrift admins (logged) before moving to real money\n+    -- when (user_id /= viewer_id) $ permissionDenied "You can only view your own account balance history."\n+\n+    Just account <- runDB $ get $ userAccount user\n+\n+    offset' <- lookupParamDefault "offset" 0\n+    limit' <- lookupParamDefault "count" 20\n+\n+    (transactions, user_accounts, project_accounts) <- runDB $ do\n+        transactions <- select $ from $ \\ transaction -> do\n+            where_ ( transaction ^. TransactionCredit ==. val (Just (userAccount user))\n+                    ||. transaction ^. TransactionDebit ==. val (Just (userAccount user)))\n+            orderBy [ desc (transaction ^. TransactionTs) ]\n+            limit limit'\n+            offset offset'\n+            return transaction\n+\n+        let accounts = catMaybes $ S.toList $ S.fromList $ map (transactionCredit . entityVal) transactions ++ map (transactionDebit . entityVal) transactions\n+\n+        users <- selectList [ UserAccount <-. accounts ] []\n+        projects <- selectList [ ProjectAccount <-. accounts ] []\n+\n+        let mkMapBy :: Ord b => (a -> b) -> [a] -> M.Map b a\n+            mkMapBy f = M.fromList . map (\\ e -> (f e, e))\n+\n+        return\n+            ( transactions\n+            , mkMapBy (userAccount . entityVal) users\n+            , mkMapBy (projectAccount . entityVal) projects\n+            )\n+\n+    (add_funds_form, _) <- generateFormPost addTestCashForm\n+\n+    defaultLayout $ do\n+        setTitle . toHtml $ "User Balance - " <> userDisplayName (Entity user_id user) <> " | Snowdrift.coop"\n+        $(widgetFile "user_balance")\n+\n+postUserBalanceR :: UserId -> Handler Html\n+postUserBalanceR user_id = do\n+    Entity viewer_id _ <- requireAuth\n+    user <- runYDB $ get404 user_id\n+    unless (user_id == viewer_id) $\n+        permissionDenied "You can only add money to your own account."\n+\n+    ((result, _), _) <- runFormPost addTestCashForm\n+\n+    now <- liftIO getCurrentTime\n+\n+    case result of\n+        FormSuccess amount -> do\n+            if amount < 10\n+                then alertDanger "Sorry, minimum deposit is $10"\n+                else do\n+                    runDB $ do\n+                        _ <- insert $ Transaction now (Just $ userAccount user) Nothing Nothing amount "Test Load" Nothing\n+                        update $ \\ account -> do\n+                            set account [ AccountBalance +=. val amount ]\n+                            where_ ( account ^. AccountId ==. val (userAccount user) )\n+\n+                    alertSuccess "Balance updated."\n+            redirect (UserBalanceR user_id)\n+\n+        _ -> error "Error processing form."\n+\n+--------------------------------------------------------------------------------\n+-- /#UserId/d\n+\n+getUserDiscussionR :: UserId -> Handler Html\n+getUserDiscussionR user_id = error "TODO(mitchell)"\n+\n+postUserDiscussionR :: UserId -> Handler Html\n+postUserDiscussionR user_id = error "TODO(mitchell)"\n+\n+--------------------------------------------------------------------------------\n+-- /#UserId/edit\n+\n+getEditUserR :: UserId -> Handler Html\n+getEditUserR user_id = do\n+    _ <- checkEditUser user_id\n+    user <- runYDB (get404 user_id)\n+\n+    (form, enctype) <- generateFormPost $ editUserForm (Just user)\n+    defaultLayout $ do\n+        setTitle . toHtml $ "User Profile - " <> userDisplayName (Entity user_id user) <> " | Snowdrift.coop"\n+        $(widgetFile "edit_user")\n+\n+postUserR :: UserId -> Handler Html\n+postUserR user_id = do\n+    viewer_id <- checkEditUser user_id\n+\n+    ((result, _), _) <- runFormPost $ editUserForm Nothing\n+\n+    case result of\n+        FormSuccess user_update -> do\n+            mode <- lookupPostParam "mode"\n+            let action :: Text = "update"\n+            case mode of\n+                Just "preview" -> do\n+                    user <- runYDB $ get404 user_id\n+\n+                    let updated_user = updateUserPreview user_update user\n+\n+                    (form, _) <- generateFormPost $ editUserForm (Just updated_user)\n+\n+                    defaultLayout $\n+                        previewWidget form action $\n+                            renderUser (Just viewer_id) user_id updated_user mempty\n+\n+                Just x | x == action -> do\n+                    runDB (updateUserDB user_id user_update)\n+                    redirect (UserR user_id)\n+\n+                _ -> do\n+                    addAlertEm "danger" "unknown mode" "Error: "\n+                    redirect (UserR user_id)\n+\n+        _ -> do\n+            alertDanger "Failed to update user."\n+            redirect (UserR user_id)\n+\n+checkEditUser :: UserId -> Handler UserId\n+checkEditUser user_id = do\n+    viewer_id <- requireAuthId\n+    unless (user_id == viewer_id) $\n+        permissionDenied "You can only modify your own profile."\n+    return viewer_id\n+\n+--------------------------------------------------------------------------------\n+-- /#UserId/elig\n+\n postUserEstEligibleR :: UserId -> Handler Html\n postUserEstEligibleR user_id = do\n     establisher_id <- requireAuthId\n@@ -153,8 +267,23 @@ postUserEstEligibleR user_id = do\n             user <- runYDB (get404 user_id)\n             case userEstablished user of\n                 EstUnestablished -> do\n-                    runSDB $ eligEstablishUser establisher_id user_id reason\n+                    runSDB (eligEstablishUserDB establisher_id user_id reason)\n                     setMessage "This user is now eligible for establishment. Thanks!"\n                     redirectUltDest HomeR\n                 _ -> error "User not unestablished!"\n         _ -> error "Error submitting form."\n+\n+--------------------------------------------------------------------------------\n+-- /#UserId/pledges\n+\n+getUserPledgesR :: UserId -> Handler Html\n+getUserPledgesR user_id = do\n+    -- TODO: refine permissions here\n+    _ <- requireAuthId\n+    user <- runYDB $ get404 user_id\n+    defaultLayout $ do\n+        setTitle . toHtml $\n+            "User Pledges - " <> userDisplayName (Entity user_id user) <> " | Snowdrift.coop"\n+\n+        $(widgetFile "user_pledges")\n+\ndiff --git a/Handler/UserBalance.hs b/Handler/UserBalance.hs\ndeleted file mode 100644\nindex 47c9d5d..0000000\n--- a/Handler/UserBalance.hs\n+++ /dev/null\n@@ -1,110 +0,0 @@\n-module Handler.UserBalance where\n-\n-import Import\n-\n-import qualified Data.Set as S\n-import qualified Data.Map as M\n-import qualified Data.Text as T\n-\n-import Model.Transaction\n-import Model.Currency\n-import Model.User\n-\n-\n-import Widgets.Time\n-\n-\n-lookupParamDefault :: Read a => Text -> a -> Handler a\n-lookupParamDefault name def = do\n-    maybe_param <- lookup name <$> reqGetParams <$> getRequest\n-    return $ fromMaybe def $ do\n-        param_str <- maybe_param\n-        param <- listToMaybe $ reads $ T.unpack param_str\n-        return $ fst param\n-\n-\n--- check permissions for user balance view\n-getUserBalanceR :: UserId -> Handler Html\n-getUserBalanceR user_id = do\n-    viewer_id <- requireAuthId\n-    if viewer_id /= user_id\n-        then permissionDenied "You must be a site administrator to view user balances."\n-        else getUserBalanceR' user_id\n-\n-getUserBalanceR' :: UserId -> Handler Html\n-getUserBalanceR' user_id = do\n-    user <- runYDB $ get404 user_id\n-\n-    -- TODO: restrict viewing balance to user or snowdrift admins (logged) before moving to real money\n-    -- when (user_id /= viewer_id) $ permissionDenied "You can only view your own account balance history."\n-\n-    Just account <- runDB $ get $ userAccount user\n-\n-    offset' <- lookupParamDefault "offset" 0\n-    limit' <- lookupParamDefault "count" 20\n-\n-    (transactions, user_accounts, project_accounts) <- runDB $ do\n-        transactions <- select $ from $ \\ transaction -> do\n-            where_ ( transaction ^. TransactionCredit ==. val (Just (userAccount user))\n-                    ||. transaction ^. TransactionDebit ==. val (Just (userAccount user)))\n-            orderBy [ desc (transaction ^. TransactionTs) ]\n-            limit limit'\n-            offset offset'\n-            return transaction\n-\n-        let accounts = catMaybes $ S.toList $ S.fromList $ map (transactionCredit . entityVal) transactions ++ map (transactionDebit . entityVal) transactions\n-\n-        users <- selectList [ UserAccount <-. accounts ] []\n-        projects <- selectList [ ProjectAccount <-. accounts ] []\n-\n-        let mkMapBy :: Ord b => (a -> b) -> [a] -> M.Map b a\n-            mkMapBy f = M.fromList . map (\\ e -> (f e, e))\n-\n-        return\n-            ( transactions\n-            , mkMapBy (userAccount . entityVal) users\n-            , mkMapBy (projectAccount . entityVal) projects\n-            )\n-\n-    (add_funds_form, _) <- generateFormPost addTestCashForm\n-\n-    defaultLayout $ do\n-        setTitle . toHtml $ "User Balance - " <> userPrintName (Entity user_id user) <> " | Snowdrift.coop"\n-        $(widgetFile "user_balance")\n-\n-\n-postUserBalanceR :: UserId -> Handler Html\n-postUserBalanceR user_id = do\n-    Entity viewer_id _ <- requireAuth\n-    user <- runYDB $ get404 user_id\n-\n-    when (user_id /= viewer_id) $ runYDB $ do\n-        is_admin <- isProjectAdmin "snowdrift" viewer_id\n-        unless is_admin $\n-            lift $ permissionDenied "You can only add money to your own account."\n-\n-    ((result, _), _) <- runFormPost addTestCashForm\n-\n-    now <- liftIO getCurrentTime\n-\n-    case result of\n-        FormSuccess amount -> do\n-            if amount < 10\n-             then\n-                addAlert "danger" "Sorry, minimum deposit is $10"\n-             else do\n-                runDB $ do\n-                    _ <- insert $ Transaction now (Just $ userAccount user) Nothing Nothing amount "Test Load" Nothing\n-                    update $ \\ account -> do\n-                        set account [ AccountBalance +=. val amount ]\n-                        where_ ( account ^. AccountId ==. val (userAccount user) )\n-\n-                addAlert "success" "Balance updated."\n-            redirect $ UserBalanceR user_id\n-\n-        _ -> error "Error processing form."\n-\n-\n-addTestCashForm :: Form Milray\n-addTestCashForm = renderBootstrap3 $ fromInteger . (10000 *) <$> areq' intField "Add (fake) money to your account (in whole dollars)" (Just 10)\n-\ndiff --git a/Handler/UserPledges.hs b/Handler/UserPledges.hs\ndeleted file mode 100644\nindex 56d6d8b..0000000\n--- a/Handler/UserPledges.hs\n+++ /dev/null\n@@ -1,20 +0,0 @@\n-module Handler.UserPledges where\n-\n-import Import\n-\n-import Model.User\n-\n-import Widgets.ProjectPledges\n-\n-\n-getUserPledgesR :: UserId -> Handler Html\n-getUserPledgesR user_id = do\n-    -- TODO: refine permissions here\n-    _ <- requireAuthId\n-    user <- runYDB $ get404 user_id\n-    defaultLayout $ do\n-        setTitle . toHtml $\n-            "User Pledges - " <> userPrintName (Entity user_id user) <> " | Snowdrift.coop"\n-\n-        $(widgetFile "user_pledges")\n-\ndiff --git a/Handler/Utils.hs b/Handler/Utils.hs\nnew file mode 100644\nindex 0000000..9408816\n--- /dev/null\n+++ b/Handler/Utils.hs\n@@ -0,0 +1,25 @@\n+module Handler.Utils where\n+\n+import Import\n+\n+import qualified Data.Text as T\n+\n+-- | Possible values for "mode" post param.\n+data PostMode\n+    = PostMode    -- Just "post"\n+    | PreviewMode -- Just "preview"\n+    | CancelMode  -- Just "cancel"\n+\n+lookupPostMode :: Handler (Maybe PostMode)\n+lookupPostMode = lookupPostParam "mode" >>= \\case\n+    Just "post"    -> return (Just PostMode)\n+    Just "preview" -> return (Just PreviewMode)\n+    Just "cancel"  -> return (Just CancelMode)\n+    _              -> return Nothing\n+\n+lookupGetUTCTimeDefaultNow :: Text -> Handler UTCTime\n+lookupGetUTCTimeDefaultNow name = lookupGetParam name >>= \\case\n+    Nothing    -> liftIO getCurrentTime\n+    Just value -> case reads (T.unpack value) of\n+        [(time,"")] -> return time\n+        _           -> liftIO getCurrentTime\ndiff --git a/Handler/Volunteer.hs b/Handler/Volunteer.hs\nindex cdee4f3..73b1aaa 100644\n--- a/Handler/Volunteer.hs\n+++ b/Handler/Volunteer.hs\n@@ -43,10 +43,10 @@ postVolunteerR project_handle = do\n                 application_id <- insert application\n                 forM_ interest_ids $ \\ interest_id -> insert $ VolunteerInterest application_id interest_id\n \n-            addAlert "success" "application submitted"\n-            redirect $ VolunteerR project_handle\n+            alertSuccess "application submitted"\n+            redirect (VolunteerR project_handle)\n \n         _ -> do\n-            addAlert "danger" "error submitting application"\n-            redirect $ VolunteerR project_handle\n+            alertDanger "error submitting application"\n+            redirect (VolunteerR project_handle)\n \ndiff --git a/Handler/Wiki.hs b/Handler/Wiki.hs\nindex 3b4a562..fb828a8 100644\n--- a/Handler/Wiki.hs\n+++ b/Handler/Wiki.hs\n@@ -4,21 +4,25 @@ module Handler.Wiki where\n \n import Import\n \n-import           Data.Tree.Extra      (sortForestBy)\n-import           Handler.Wiki.Comment (getMaxDepth, processWikiComment)\n+import           Handler.Comment\n+import           Handler.Discussion\n+import           Handler.Wiki.Comment (makeWikiPageCommentForestWidget)\n import           Model.Comment\n+import           Model.Comment.ActionPermissions\n+import           Model.Comment.Sql\n+import           Model.Discussion\n import           Model.Markdown\n-import           Model.Message\n+import           Model.Notification\n import           Model.Permission\n-import           Model.Project\n-import           Model.Tag            (getAllTagsMap)\n import           Model.User\n+import           Model.Wiki\n import           Widgets.Preview\n import           Widgets.Time\n import           View.Comment\n import           View.Wiki\n \n import           Data.Algorithm.Diff  (getDiff, Diff (..))\n+import           Data.Default         (def)\n import qualified Data.Map             as M\n import qualified Data.Set             as S\n import qualified Data.Text            as T\n@@ -29,30 +33,51 @@ import           Yesod.Markdown\n --------------------------------------------------------------------------------\n -- Utility functions\n \n-getPageInfo :: Text -> Text -> YDB (Entity Project, Entity WikiPage)\n-getPageInfo project_handle target = do\n+-- | Get the Project/WikiPage entities.\n+pageInfo :: Text -> Text -> YDB (Entity Project, Entity WikiPage)\n+pageInfo project_handle target = do\n     project <- getBy404 $ UniqueProjectHandle project_handle\n     page    <- getBy404 $ UniqueWikiTarget (entityKey project) target\n     return (project, page)\n \n---------------------------------------------------------------------------------\n--- /\n-\n-getWikiPagesR :: Text -> Handler Html\n-getWikiPagesR project_handle = do\n-    (project, pages) <- runYDB $ do\n-        Entity project_id project <- getBy404 $ UniqueProjectHandle project_handle\n-        pages <- getProjectWikiPages project_id\n-        return (project, pages)\n-    defaultLayout $ do\n-        setTitle . toHtml $ projectName project <> " Wiki | Snowdrift.coop"\n-        $(widgetFile "wiki_pages")\n-\n--- | Redirect 'here' with added GET parameters.\n-redirectHereWithParams :: [(Text, Text)] -> Handler a\n-redirectHereWithParams new_params = do\n-    Just route <- getCurrentRoute\n-    getRequest >>= redirectParams route . (new_params ++) . reqGetParams\n+-- | Get the Project/WikiPage entities, but require some generic permissions,\n+-- failing with permissionDenied if they are not satisfied.\n+pageInfoRequirePermission :: Text                                                          -- Project handle.\n+                          -> Text                                                          -- Wiki page.\n+                          -> (Entity User -> Entity Project -> Entity WikiPage -> DB Bool) -- Permission checker.\n+                          -> Handler (Entity User, Entity Project, Entity WikiPage)\n+pageInfoRequirePermission project_handle target has_permission = do\n+    user <- requireAuth\n+    (project, page, ok) <- runYDB $ do\n+        project <- getBy404 (UniqueProjectHandle project_handle)\n+        page    <- getBy404 (UniqueWikiTarget (entityKey project) target)\n+        ok <- has_permission user project page\n+        return (project, page, ok)\n+    unless ok (permissionDenied "You don't have permission to access this page.")\n+    return (user, project, page)\n+\n+-- | Like pageInfoRequirePermission, but specialized to requiring Project affiliation.\n+pageInfoRequireAffiliation :: Text -> Text -> Handler (Entity User, Entity Project, Entity WikiPage)\n+pageInfoRequireAffiliation project_handle target =\n+    pageInfoRequirePermission project_handle target (\\(Entity user_id _) (Entity project_id _) _ ->\n+      userIsAffiliatedWithProjectDB user_id project_id)\n+\n+-- | Like pageInfoRequireAffiliation, but this is for creating a new WikiPage, so one doesn't\n+-- already exist. TODO(mitchell): Make a better abstraction here.\n+pageInfoRequireAffiliation' :: Text -> Handler (Entity User, Entity Project)\n+pageInfoRequireAffiliation' project_handle = do\n+    user@(Entity user_id _) <- requireAuth\n+    (project, ok) <- runYDB $ do\n+        project@(Entity project_id _) <- getBy404 (UniqueProjectHandle project_handle)\n+        ok <- userIsAffiliatedWithProjectDB user_id project_id\n+        return (project, ok)\n+    unless ok (permissionDenied "You don't have permission to access this page.")\n+    return (user, project)\n+\n+-- | Like pageInfoRequirePermission, but specialized to requiring that the User can edit a WikiPage.\n+pageInfoRequireCanEdit :: Text -> Text -> Handler (Entity User, Entity Project, Entity WikiPage)\n+pageInfoRequireCanEdit project_handle target =\n+    pageInfoRequirePermission project_handle target (\\(Entity _ user) _ _ -> return (userCanEditWikiPage user))\n \n --------------------------------------------------------------------------------\n -- /#target\n@@ -60,35 +85,37 @@ redirectHereWithParams new_params = do\n getWikiR :: Text -> Text -> Handler Html\n getWikiR project_handle target = do\n     maybe_user <- maybeAuth\n-    (Entity project_id project, Entity _ page) <- runYDB $ getPageInfo project_handle target\n+    (project, page, comment_count) <- runYDB $ do\n+        (Entity project_id project, Entity page_id page) <- pageInfo project_handle target\n \n-    comment_count <- runDB $ do\n         let muser_id      = entityKey <$> maybe_user\n             discussion_id = wikiPageDiscussion page\n-        roots_ids <- map entityKey <$> getAllOpenRootComments muser_id project_id discussion_id\n-        children <- getCommentsDescendants muser_id project_id roots_ids\n-        return $ length roots_ids + length children\n \n-    let can_edit = fromMaybe False (isEstablished . entityVal <$> maybe_user)\n+        case muser_id of\n+            Nothing -> return ()\n+            Just user_id -> do\n+                is_watching <- userIsWatchingProjectDB user_id project_id\n+                when is_watching $\n+                    userViewWikiEditsDB user_id page_id\n \n-    defaultLayout $ do\n+        let has_permission = exprCommentProjectPermissionFilter muser_id (val project_id)\n+        roots_ids    <- map entityKey <$> fetchDiscussionRootCommentsDB discussion_id has_permission\n+        num_children <- length <$> fetchCommentsDescendantsDB roots_ids has_permission\n+        return (project, page, length roots_ids + num_children)\n+\n+    let can_edit = fromMaybe False (userCanEditWikiPage . entityVal <$> maybe_user)\n \n+    defaultLayout $ do\n         setTitle . toHtml $\n             projectName project <> " : " <> wikiPageTarget page <> " | Snowdrift.coop"\n \n-        renderWiki comment_count project_handle target can_edit True page\n+        renderWiki comment_count project_handle target can_edit page\n \n postWikiR :: Text -> Text -> Handler Html\n postWikiR project_handle target = do\n-    Entity user_id user <- requireAuth\n     now <- liftIO getCurrentTime\n \n-    let can_edit = isEstablished user\n-\n-    unless can_edit $ permissionDenied "you do not have permission to edit this page"\n-\n-    (Entity project_id _, Entity page_id page) <- runYDB $ getPageInfo project_handle target\n-\n+    (Entity user_id _, Entity project_id _, Entity page_id page) <- pageInfoRequireCanEdit project_handle target\n     Entity _ last_edit <- runYDB $ getBy404 $ UniqueWikiLastEdit page_id\n \n     ((result, _), _) <- runFormPost $ editWikiForm (wikiLastEditEdit last_edit) (wikiPageContent page) Nothing\n@@ -105,8 +132,8 @@ postWikiR project_handle target = do\n                     (form, _) <- generateFormPost $ editWikiForm last_edit_id content (Just comment)\n \n                     defaultLayout $ previewWidget form action $\n-                        renderWiki 0 project_handle target False False $\n-                            WikiPage target project_id content (Key $ PersistInt64 (-1)) Normal\n+                        renderWiki 0 project_handle target False $\n+                            WikiPage now target project_id content (Key $ PersistInt64 (-1)) Normal\n \n                 Just x | x == action -> do\n                     runSYDB $ do\n@@ -115,12 +142,12 @@ postWikiR project_handle target = do\n                             set p [WikiPageContent =. val content]\n                             where_ $ p ^. WikiPageId ==. val page_id\n \n-                        edit_id <- lift $ insert $ WikiEdit now user_id page_id content (Just comment)\n+                        edit_id <- createWikiEditDB user_id page_id content (Just comment)\n                         -- TODO - I think there might be a race condition here...\n                         either_last_edit <- lift $ insertBy $ WikiLastEdit page_id edit_id\n \n                         if last_edit_id == wikiLastEditEdit last_edit\n-                         then lift $ lift $ addAlert "success" "Updated."\n+                         then lift (lift (alertSuccess "Updated."))\n                          else do\n                             [ Value last_editor ] <- lift $\n                                 select $\n@@ -140,20 +167,20 @@ postWikiR project_handle target = do\n                                     , "(this ticket was automatically generated)"\n                                     ]\n \n-                            comment_id <- lift $ insert =<< makeModeratedComment user_id (wikiPageDiscussion page) Nothing comment_body 0\n+                            comment_id <- lift $ insert =<< makeApprovedComment user_id (wikiPageDiscussion page) Nothing comment_body 0\n \n                             lift $ insert_ $ Ticket now now "edit conflict" comment_id\n \n                             render <- lift getUrlRenderParams\n-                            let message_text = Markdown $ T.unlines\n+                            let notif_text = Markdown $ T.unlines\n                                     [ "Edit conflict for wiki page *" <> target <> "*."\n-                                    , "<br>[**Ticket created**](" <> render (DiscussCommentR project_handle target comment_id) [] <> ")"\n+                                    , "<br>[**Ticket created**](" <> render (WikiCommentR project_handle target comment_id) [] <> ")"\n                                     ]\n \n-                            insertMessage_ MessageDirect (Just project_id) (Just last_editor) (Just user_id)     message_text True\n-                            insertMessage_ MessageDirect (Just project_id) (Just user_id)     (Just last_editor) message_text True\n+                            sendNotificationDB_ NotifEditConflict last_editor Nothing notif_text\n+                            sendNotificationDB_ NotifEditConflict user_id     Nothing notif_text\n \n-                            lift $ lift $ addAlert "danger" "conflicting edits (ticket created, messages sent)"\n+                            lift (lift (alertDanger "conflicting edits (ticket created, notification sent)"))\n \n                         case either_last_edit of\n                             Left (Entity to_update _) -> lift $\n@@ -174,63 +201,44 @@ postWikiR project_handle target = do\n --------------------------------------------------------------------------------\n -- /#target/d\n \n--- | getDiscussWikiR generates the associated discussion page for each wiki page\n-getDiscussWikiR :: Text -> Text -> Handler Html\n-getDiscussWikiR project_handle target = lookupGetParam "state" >>= \\case\n-    Just "closed" -> go getAllClosedRootComments\n-    _             -> go getAllOpenRootComments\n-  where\n-    go = getDiscussWikiR' project_handle target\n-\n-getDiscussWikiR' :: Text                      -- ^ Project handle.\n-                 -> Text                      -- ^ Wiki page name.\n-                 -> (Maybe UserId\n-                     -> ProjectId\n-                     -> DiscussionId\n-                     -> DB [Entity Comment])  -- ^ Root comment getter.\n-                 -> Handler Html\n-getDiscussWikiR' project_handle target get_root_comments = do\n+-- | getWikiDiscussionR generates the associated discussion page for each wiki page\n+getWikiDiscussionR :: Text -> Text -> Handler Html\n+getWikiDiscussionR project_handle target = getDiscussion (getWikiDiscussionR' project_handle target)\n+\n+getWikiDiscussionR'\n+        :: Text                                                      -- ^ Project handle.\n+        -> Text                                                      -- ^ Wiki page name.\n+        -> (DiscussionId -> ExprCommentCond -> DB [Entity Comment])  -- ^ Root comment getter.\n+        -> Handler Html\n+getWikiDiscussionR' project_handle target get_root_comments = do\n     muser <- maybeAuth\n     let muser_id = entityKey <$> muser\n \n-    (Entity project_id project, Entity _ page) <- runYDB $ getPageInfo project_handle target\n-\n-    (roots, replies, user_map, closure_map, ticket_map, flag_map, tag_map) <- runDB $ do\n-        roots           <- get_root_comments muser_id project_id (wikiPageDiscussion page)\n-        replies         <- getCommentsDescendants muser_id project_id (map entityKey roots)\n-        user_map        <- entitiesMap <$> fetchUsersInDB (S.toList $ getCommentsUsers roots <> getCommentsUsers replies)\n-        let comment_ids  = map entityKey (roots ++ replies)\n-        closure_map     <- makeClosureMap comment_ids\n-        ticket_map      <- makeTicketMap  comment_ids\n-        flag_map        <- makeFlagMap    comment_ids\n-        tag_map         <- getAllTagsMap\n-        return (roots, replies, user_map, closure_map, ticket_map, flag_map, tag_map)\n-\n-    max_depth <- getMaxDepth\n-\n-    -- TODO(mitchell): use makeCommentWidget here\n-    let comments = do\n-            forM_ (sortForestBy orderingNewestFirst (buildCommentForest roots replies)) $ \\comment ->\n-                commentTreeWidget\n-                    mempty\n-                    comment\n-                    [] -- earlier closures\n-                    user_map\n-                    closure_map\n-                    ticket_map\n-                    flag_map\n-                    tag_map\n-                    project_handle\n-                    target\n-                    True           -- show actions?\n-                    max_depth\n-                    0              -- depth\n-            toWidget $(cassiusFile "templates/comment_wrapper.cassius")\n+    (Entity project_id project, page, root_comments) <- runYDB $ do\n+        (project@(Entity project_id _), Entity _ page) <- pageInfo project_handle target\n+        let has_permission = (exprCommentProjectPermissionFilter muser_id (val project_id))\n+        root_comments <- get_root_comments (wikiPageDiscussion page) has_permission\n+        return (project, page, root_comments)\n+\n+    (comment_forest_no_css, _) <-\n+        makeWikiPageCommentForestWidget\n+          muser\n+          project_id\n+          project_handle\n+          (wikiPageTarget page)\n+          root_comments\n+          def\n+          (getMaxDepthDefault 0)\n+          False\n+          mempty\n+\n+    let has_comments = not (null root_comments)\n+        comment_forest = do\n+            comment_forest_no_css\n+            toWidget $(cassiusFile "templates/comment.cassius")\n \n     (comment_form, _) <- generateFormPost commentNewTopicForm\n \n-    let has_comments = not $ null roots\n-\n     defaultLayout $ do\n         setTitle . toHtml $ projectName project <> " Wiki Discussion - " <> target <> " | Snowdrift.coop"\n         $(widgetFile "wiki_discuss")\n@@ -238,24 +246,24 @@ getDiscussWikiR' project_handle target get_root_comments = do\n --------------------------------------------------------------------------------\n -- /#target/d/new\n \n-getNewDiscussWikiR :: Text -> Text -> Handler Html\n-getNewDiscussWikiR project_handle target = do\n+getNewWikiDiscussionR :: Text -> Text -> Handler Html\n+getNewWikiDiscussionR project_handle target = do\n     void requireAuth\n-    (comment_form, _) <- generateFormPost commentNewTopicForm\n-    defaultLayout $(widgetFile "wiki_discuss_new")\n-\n-postNewDiscussWikiR :: Text -> Text -> Handler Html\n-postNewDiscussWikiR project_handle target = do\n-    (project_entity, Entity _ page) <- runYDB $ getPageInfo project_handle target\n-\n-    ((result, _), _) <- runFormPost commentNewTopicForm\n-\n-    case result of\n-        FormSuccess text -> do\n-            mode <- lookupPostParam "mode"\n-            processWikiComment mode Nothing text project_entity page\n-        FormMissing      -> error "Form missing."\n-        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\\n" msgs)\n+    let widget = commentNewTopicFormWidget\n+    defaultLayout $(widgetFile "wiki_discussion_wrapper")\n+\n+postNewWikiDiscussionR :: Text -> Text -> Handler Html\n+postNewWikiDiscussionR project_handle target = do\n+    user <- requireAuth\n+    (_, Entity _ WikiPage{..}) <- runYDB (pageInfo project_handle target)\n+\n+    postNewComment\n+      Nothing\n+      user\n+      wikiPageDiscussion\n+      (makeProjectCommentActionPermissions project_handle) >>= \\case\n+        Left comment_id -> redirect (WikiCommentR project_handle target comment_id)\n+        Right (widget, form) -> defaultLayout $ previewWidget form "post" ($(widgetFile "wiki_discussion_wrapper"))\n \n --------------------------------------------------------------------------------\n -- /#target/diff/#from/#to\n@@ -263,7 +271,7 @@ postNewDiscussWikiR project_handle target = do\n \n getWikiDiffR :: Text -> Text -> WikiEditId -> WikiEditId -> Handler Html\n getWikiDiffR project_handle target start_edit_id end_edit_id = do\n-    (Entity _ project, Entity page_id _) <- runYDB $ getPageInfo project_handle target\n+    (Entity _ project, Entity page_id _) <- runYDB $ pageInfo project_handle target\n \n     (start_edit, end_edit) <- runYDB $ (,)\n         <$> get404 start_edit_id\n@@ -302,15 +310,9 @@ getWikiDiffProxyR project_handle target = do\n \n getEditWikiR :: Text -> Text -> Handler Html\n getEditWikiR project_handle target = do\n-    user <- entityVal <$> requireAuth\n-    (Entity _ project, Entity page_id page) <- runYDB $ getPageInfo project_handle target\n-\n+    (_, Entity _ project, Entity page_id page) <- pageInfoRequireCanEdit project_handle target\n     Entity _ last_edit <- runYDB $ getBy404 $ UniqueWikiLastEdit page_id\n \n-    let can_edit = isEstablished user\n-\n-    unless can_edit $ permissionDenied "you do not have permission to edit this page"\n-\n     (wiki_form, _) <- generateFormPost $ editWikiForm (wikiLastEditEdit last_edit) (wikiPageContent page) Nothing\n \n     defaultLayout $ do\n@@ -322,7 +324,7 @@ getEditWikiR project_handle target = do\n \n getWikiHistoryR :: Text -> Text -> Handler Html\n getWikiHistoryR project_handle target = do\n-    (Entity _ project, Entity page_id _) <- runYDB $ getPageInfo project_handle target\n+    (Entity _ project, Entity page_id _) <- runYDB $ pageInfo project_handle target\n \n     (edits, users) <- runDB $ do\n         edits <-\n@@ -350,7 +352,7 @@ getWikiHistoryR project_handle target = do\n \n getWikiEditR :: Text -> Text -> WikiEditId -> Handler Html\n getWikiEditR project_handle target edit_id = do\n-    (Entity _ project, Entity page_id _) <- runYDB $ getPageInfo project_handle target\n+    (Entity _ project, Entity page_id _) <- runYDB $ pageInfo project_handle target\n     edit <- runYDB $ do\n         edit <- get404 edit_id\n \n@@ -368,16 +370,8 @@ getWikiEditR project_handle target edit_id = do\n \n getNewWikiR :: Text -> Text -> Handler Html\n getNewWikiR project_handle target = do\n-    user_id <- requireAuthId\n-    Entity _ project <- runYDB $ getBy404 $ UniqueProjectHandle project_handle\n-    affiliated <- runDB $ (||)\n-            <$> isProjectAffiliated project_handle user_id\n-            <*> isProjectAdmin "snowdrift" user_id\n-\n-    unless affiliated $ permissionDenied "you do not have permission to edit this page"\n-\n+    (_, Entity _ project) <- pageInfoRequireAffiliation' project_handle\n     (wiki_form, _) <- generateFormPost $ newWikiForm Nothing\n-\n     defaultLayout $ do\n         setTitle . toHtml $ projectName project <> " Wiki - New Page | Snowdrift.coop"\n         $(widgetFile "new_wiki")\n@@ -385,21 +379,9 @@ getNewWikiR project_handle target = do\n \n postNewWikiR :: Text -> Text -> Handler Html\n postNewWikiR project_handle target = do\n-    Entity user_id _ <- requireAuth\n-\n-    affiliated <- runDB $ (||)\n-            <$> isProjectAffiliated project_handle user_id\n-            <*> isProjectAdmin "snowdrift" user_id\n-\n-    unless affiliated $\n-        permissionDenied "you do not have permission to edit this page"\n-\n+    (Entity user_id _, Entity project_id _) <- pageInfoRequireAffiliation' project_handle\n     now <- liftIO getCurrentTime\n-\n-    Entity project_id _ <- runYDB . getBy404 $ UniqueProjectHandle project_handle\n-\n     ((result, _), _) <- runFormPost $ newWikiForm Nothing\n-\n     case result of\n         FormSuccess content -> do\n             mode <- lookupPostParam "mode"\n@@ -408,17 +390,13 @@ postNewWikiR project_handle target = do\n                 Just "preview" -> do\n                         (form, _) <- generateFormPost $ newWikiForm (Just content)\n                         defaultLayout $ do\n-                            let page = WikiPage target project_id content (Key $ PersistInt64 0) Normal\n-                            previewWidget form action $ renderWiki 0 project_handle target False False page\n+                            let page = WikiPage now target project_id content (Key $ PersistInt64 0) Normal\n+                            previewWidget form action $ renderWiki 0 project_handle target False page\n \n                 Just x | x == action -> do\n-                    _ <- runDB $ do\n-                        discussion <- insert (Discussion 0)\n-                        page_id <- insert $ WikiPage target project_id content discussion Normal\n-                        edit_id <- insert $ WikiEdit now user_id page_id content $ Just "Page created."\n-                        insert $ WikiLastEdit page_id edit_id\n+                    runSDB (createWikiPageDB target project_id content Normal user_id)\n \n-                    addAlert "success" "Created."\n+                    alertSuccess "Created."\n                     redirect $ WikiR project_handle target\n \n                 _ -> error "unrecognized mode"\n@@ -426,22 +404,12 @@ postNewWikiR project_handle target = do\n         FormMissing -> error "Form missing."\n         FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.concat msgs)\n \n-\n-\n --------------------------------------------------------------------------------\n -- /#target/perm\n \n getEditWikiPermissionsR :: Text -> Text -> Handler Html\n getEditWikiPermissionsR project_handle target = do\n-    user_id <- requireAuthId\n-    (Entity _ project, Entity _ page) <- runYDB $ getPageInfo project_handle target\n-\n-    affiliated <- runDB $ (||)\n-            <$> isProjectAdmin project_handle user_id\n-            <*> isProjectAdmin "snowdrift" user_id\n-\n-    unless affiliated $ permissionDenied "you do not have permission to edit page permissions"\n-\n+    (_, Entity _ project, Entity _ page) <- pageInfoRequireAffiliation project_handle target\n     (wiki_form, _) <- generateFormPost $ editWikiPermissionsForm (wikiPagePermissionLevel page)\n \n     defaultLayout $ do\n@@ -450,15 +418,7 @@ getEditWikiPermissionsR project_handle target = do\n \n postEditWikiPermissionsR :: Text -> Text -> Handler Html\n postEditWikiPermissionsR project_handle target = do\n-    Entity user_id _ <- requireAuth\n-    (_, Entity page_id page) <- runYDB $ getPageInfo project_handle target\n-\n-    affiliated <- runDB $ (||)\n-            <$> isProjectAdmin project_handle user_id\n-            <*> isProjectAdmin "snowdrift" user_id\n-\n-    unless affiliated $ permissionDenied "you do not have permission to edit page permissions"\n-\n+    (_, _, Entity page_id page) <- pageInfoRequireAffiliation project_handle target\n     ((result, _), _) <- runFormPost $ editWikiPermissionsForm (wikiPagePermissionLevel page)\n \n     case result of\n@@ -468,7 +428,7 @@ postEditWikiPermissionsR project_handle target = do\n                 where_ $ p ^. WikiPageId ==. val page_id\n                 set p [ WikiPagePermissionLevel =. val level ]\n \n-            addAlert "success" "permissions updated"\n+            alertSuccess "permissions updated"\n \n             redirect $ WikiR project_handle target\n \n@@ -485,5 +445,3 @@ postEditWikiPermissionsR project_handle target = do\n -- almost certainly internal anyway)\n getOldWikiEditR :: Text -> Text -> WikiEditId -> Handler Html\n getOldWikiEditR project_handle target edit_id = redirect $ WikiEditR project_handle target edit_id\n-\n-\ndiff --git a/Handler/Wiki/Comment.hs b/Handler/Wiki/Comment.hs\nindex 916be45..ab7909a 100644\n--- a/Handler/Wiki/Comment.hs\n+++ b/Handler/Wiki/Comment.hs\n@@ -4,955 +4,423 @@ module Handler.Wiki.Comment where\n \n import Import\n \n-import qualified Data.Tree.Extra           as Tree\n-import           Data.Tree.Extra           (sortTreeBy)\n-import           Model.AnnotatedTag\n+import           Handler.Comment\n import           Model.Comment\n-import           Model.Project             (getProjectTagList)\n-import           Model.Tag                 (TagMap, getAllTags)\n+import           Model.Comment.ActionPermissions\n+import           Model.Comment.HandlerInfo\n+import           Model.Comment.Sql\n import           Model.User\n import           Widgets.Preview\n-import           Widgets.Tag\n-import           View.Comment\n-\n-import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)\n-import           Data.Default              (Default, def)\n-import qualified Data.Map                  as M\n-import qualified Data.Set                  as S\n-import qualified Data.Text                 as T\n-import           Data.Tree                 (Tree)\n-import qualified Data.Tree                 as Tree\n-import           Network.HTTP.Types.Status (movedPermanently301)\n-import           Yesod.Default.Config\n-import           Yesod.Markdown\n \n+import           Data.Default                    (def)\n+import           Data.Tree                       (Forest, Tree)\n+import qualified Data.Tree                       as Tree\n+import           Text.Cassius (cassiusFile)\n \n --------------------------------------------------------------------------------\n -- Utility functions\n \n-redirectIfRethreaded :: Text -> CommentId -> Handler ()\n-redirectIfRethreaded project_handle comment_id = runDB go >>= \\case\n-    Nothing -> return ()\n-    Just (destination_comment_id, target) ->\n-        redirectWith movedPermanently301 (DiscussCommentR project_handle target destination_comment_id)\n-  where\n-    go :: DB (Maybe (CommentId, Text))\n-    go = runMaybeT $ do\n-        destination_comment_id <- MaybeT (getCommentRethread comment_id)\n-        target                 <- lift (wikiPageTarget <$> getCommentPage destination_comment_id)\n-        return (destination_comment_id, target)\n-\n -- | Convenience method for all pages that accept a project handle, target, and comment id\n -- as URL parameters. Makes sure that the comment is indeed on the page. Redirects if the\n--- comment was rethreaded.\n-checkCommentPage :: Text -> Text -> CommentId -> Handler (Entity Project, Entity WikiPage, Comment)\n+-- comment was rethreaded. 404's if the comment doesn't exist. 403 if permission denied.\n+checkCommentPage :: Text -> Text -> CommentId -> Handler (Maybe (Entity User), Entity Project, Entity WikiPage, Comment)\n checkCommentPage project_handle target comment_id = do\n-    redirectIfRethreaded project_handle comment_id\n-\n-    (project@(Entity project_id _), page, comment) <- runYDB $ do\n-        project <- getBy404 $ UniqueProjectHandle project_handle\n-        page    <- getBy404 $ UniqueWikiTarget (entityKey project) target\n-        comment <- get404 comment_id\n-        return (project, page, comment)\n-\n-    when (commentDiscussion comment /= wikiPageDiscussion (entityVal page)) $\n-        error "comment does not match page"\n-\n-    -- The logic here is DUPLICATED (in SQL land) in Model.Comment.exprPermissionFilter!\n-    -- (because this function only fetches the root comment via Database.Persist.get) - all\n-    -- changes here must be reflected there, too!\n-\n-    maybeAuthId >>= \\case\n-        -- Not logged in: only show approved-and-not-flagged comments\n-        Nothing -> do\n-            ok <- isApprovedAndNotFlagged comment\n-            unless ok deny\n-        -- Logged in:\n-        Just viewer_id -> do\n-            -- If mod: no restrictions (show unapproved and flagged)\n-            is_mod <- runDB $ isProjectModerator' viewer_id project_id\n-            unless is_mod $\n-                -- Otherwise, if ordinary user, if viewing own comment, no restrictions.\n-                unless (commentUser comment == viewer_id) $ do\n-                    -- Otherwise, same restriction as not logged in applies:\n-                    -- only show approved-and-not-flagged comments.\n-                    ok <- isApprovedAndNotFlagged comment\n-                    unless ok deny\n-\n-    return (project, page, comment)\n-  where\n-    isApprovedAndNotFlagged :: Comment -> Handler Bool\n-    isApprovedAndNotFlagged comment =\n-        if isApproved comment\n-            then not <$> runDB (isFlagged comment_id)\n-            else return False\n-\n-    deny :: Handler a\n-    deny = permissionDenied "You don't have permission to view this comment."\n-\n-requireModerator :: Text -> Text -> UserId -> Handler ()\n-requireModerator message project_handle user_id = do\n-    ok <- runDB $ isProjectModerator project_handle user_id\n-    unless ok $\n-        permissionDenied message\n-\n-processWikiComment :: Maybe Text -> Maybe CommentId -> Markdown -> Entity Project -> WikiPage -> Handler Html\n-processWikiComment mode =\n-    case mode of\n-        Just "preview" -> processWikiCommentPreview\n-        Just "post"    -> processWikiCommentPost\n-        _              -> error $ "Error: unrecognized mode (" ++ show mode ++ ")"\n-\n--- TODO(mitchell): We should reuse makeCommentWidget here, somehow.\n-processWikiCommentPreview :: Maybe CommentId -> Markdown -> Entity Project -> WikiPage -> Handler Html\n-processWikiCommentPreview maybe_parent_id text (Entity _ project) page = do\n-    Entity user_id user <- requireAuth\n-\n-    (earlier_closures, tag_map) <- runDB $ (,)\n-        <$> maybe (return []) getAncestorClosures' maybe_parent_id\n-        <*> (entitiesMap <$> getAllTags)\n-\n-    depth <- depthFromMaybeParentId maybe_parent_id\n-    now <- liftIO getCurrentTime\n-    let (moderated_ts, moderated_by) = if isEstablished user then (Just now, Just user_id) else (Nothing, Nothing)\n-\n-        comment =\n-          Entity (Key $ PersistInt64 0) $\n-            Comment now moderated_ts moderated_by (wikiPageDiscussion page) maybe_parent_id user_id text depth\n-\n-        rendered_comment = let project_handle = projectHandle project\n-                               target         = wikiPageTarget page\n-                               comment_widget = commentTreeWidget\n-                                                  mempty\n-                                                  (Tree.singleton comment)\n-                                                  earlier_closures\n-                                                  (M.singleton user_id user)\n-                                                  mempty -- closure map - TODO(mitchell): this isn't right...?\n-                                                  mempty -- ticket map - TODO(mitchell): this isn't right either\n-                                                  mempty -- flag map\n-                                                  tag_map\n-                                                  (projectHandle project)\n-                                                  (wikiPageTarget page)\n-                                                  False   -- show actions?\n-                                                  0\n-                                                  0\n-                           in $(widgetFile "comment_wrapper")\n-\n-    (form, _) <- generateFormPost $\n-        commentForm\n-          (maybe "New Topic" (const "Reply") maybe_parent_id)\n-          (Just text)\n-    defaultLayout $ previewWidget form "post" rendered_comment\n-\n-processWikiCommentPost :: Maybe CommentId -> Markdown -> Entity Project -> WikiPage -> Handler Html\n-processWikiCommentPost maybe_parent_id text (Entity _ project) page = do\n-    Entity user_id user <- requireAuth\n-    now <- liftIO getCurrentTime\n-    depth <- depthFromMaybeParentId maybe_parent_id\n-\n-    let is_established = isEstablished user\n-    maybe_parent_id' <- runSYDB $ do\n-        maybe_parent_id' <- lift $ maybe (return Nothing) (fmap Just . getCommentDestination) maybe_parent_id\n-\n-        comment_id <-\n-            if is_established\n-                then insertApprovedComment   now now user_id (wikiPageDiscussion page) maybe_parent_id' user_id text depth\n-                else insertUnapprovedComment now             (wikiPageDiscussion page) maybe_parent_id' user_id text depth\n-\n-        let content = T.lines $ (\\ (Markdown str) -> str) text\n-            tickets = map T.strip $ mapMaybe (T.stripPrefix "ticket:") content\n-            tags    = map T.strip $ mconcat $ map (T.splitOn ",") $ mapMaybe (T.stripPrefix "tags:") content\n-\n-        lift $ forM_ tickets $ \\ticket -> insert_ $ Ticket now now ticket comment_id\n-        lift $ forM_ tags $ \\tag -> do\n-            tag_id <- fmap (either entityKey id) $ insertBy $ Tag tag\n-            insert_ $ CommentTag comment_id tag_id user_id 1\n-\n-        ancestor_ids <- lift $ maybe (return [])\n-                                      (\\parent_id -> (parent_id :) <$> getCommentAncestors parent_id)\n-                                      maybe_parent_id\n-\n-        lift $ forM_ ancestor_ids (insert_ . CommentAncestor comment_id)\n-\n-        lift $ update $ \\ticket -> do\n-                  set ticket [ TicketUpdatedTs =. val now ]\n-                  where_ $ ticket ^. TicketComment `in_` subGetCommentAncestors comment_id\n-\n-        return maybe_parent_id'\n-\n-    addAlert "success" $ if is_established then "comment posted" else "comment submitted for moderation"\n-    redirect $ maybe (DiscussWikiR (projectHandle project) (wikiPageTarget page)) (DiscussCommentR (projectHandle project) (wikiPageTarget page)) maybe_parent_id'\n-\n--- Get the depth of a comment, given (maybe) its parent's CommentId.\n-depthFromMaybeParentId :: Maybe CommentId -> Handler Int\n-depthFromMaybeParentId = maybe (return 0) (\\c ->  fmap (+1) $ runDB $ getCommentDepth c)\n-\n-getMaxDepth :: Handler Int\n-getMaxDepth = getMaxDepthDefault 11 -- hard-coded default max depth\n-\n-getMaxDepthZero :: Handler Int\n-getMaxDepthZero = getMaxDepthDefault 0\n-\n-getMaxDepthDefault :: Int -> Handler Int\n-getMaxDepthDefault n = fromMaybe n <$> runInputGet (iopt intField "maxdepth")\n+    muser <- maybeAuth\n+    (project, page, comment) <- checkCommentPage' (entityKey <$> muser) project_handle target comment_id\n+    return (muser, project, page, comment)\n+\n+-- | Like checkCommentPage, but authentication is required.\n+checkCommentPageRequireAuth :: Text -> Text -> CommentId -> Handler (Entity User, Entity Project, Entity WikiPage, Comment)\n+checkCommentPageRequireAuth project_handle target comment_id = do\n+    user@(Entity user_id _) <- requireAuth\n+    (project, page, comment) <- checkCommentPage' (Just user_id) project_handle target comment_id\n+    return (user, project, page, comment)\n+\n+-- | Abstract checkCommentPage and checkCommentPageRequireAuth. You shouldn't\n+-- use this function directly.\n+checkCommentPage' :: Maybe UserId -> Text -> Text -> CommentId -> Handler (Entity Project, Entity WikiPage, Comment)\n+checkCommentPage' muser_id project_handle target comment_id = do\n+    redirectIfRethreaded comment_id\n+\n+    (project, page, ecomment) <- runYDB $ do\n+        project@(Entity project_id _) <- getBy404 (UniqueProjectHandle project_handle)\n+        page <- getBy404 (UniqueWikiTarget (entityKey project) target)\n+        let has_permission = exprCommentProjectPermissionFilter muser_id (val project_id)\n+        ecomment <- fetchCommentDB comment_id has_permission\n+        return (project, page, ecomment)\n+\n+    case ecomment of\n+        Left CommentNotFound         -> notFound\n+        Left CommentPermissionDenied -> permissionDenied "You don't have permission to view this comment."\n+        Right comment                ->\n+            if commentDiscussion comment /= wikiPageDiscussion (entityVal page)\n+                then notFound\n+                else return (project, page, comment)\n+\n+checkWikiPageCommentActionPermission\n+        :: (CommentActionPermissions -> Bool)\n+        -> Text\n+        -> Entity Comment\n+        -> Handler ()\n+checkWikiPageCommentActionPermission can_perform_action project_handle comment = do\n+    ok <- can_perform_action <$> makeProjectCommentActionPermissions project_handle comment\n+    unless ok (permissionDenied "You don't have permission to perform this action.")\n+\n+makeWikiPageCommentForestWidget\n+        :: Maybe (Entity User)\n+        -> ProjectId\n+        -> Text\n+        -> Text\n+        -> [Entity Comment]\n+        -> CommentMods\n+        -> Handler MaxDepth\n+        -> Bool\n+        -> Widget\n+        -> Handler (Widget, Forest (Entity Comment))\n+makeWikiPageCommentForestWidget\n+        muser\n+        project_id\n+        project_handle\n+        target\n+        comments\n+        comment_mods\n+        get_max_depth\n+        is_preview\n+        widget_under_root_comment = do\n+    makeCommentForestWidget\n+      (wikiPageCommentHandlerInfo (entityKey <$> muser) project_id project_handle target)\n+      comments\n+      muser\n+      comment_mods\n+      get_max_depth\n+      is_preview\n+      widget_under_root_comment\n+\n+makeWikiPageCommentTreeWidget\n+        :: Maybe (Entity User)\n+        -> ProjectId\n+        -> Text\n+        -> Text\n+        -> Entity Comment\n+        -> CommentMods\n+        -> Handler MaxDepth\n+        -> Bool\n+        -> Widget\n+        -> Handler (Widget, Tree (Entity Comment))\n+makeWikiPageCommentTreeWidget a b c d e f g h i = do\n+    (widget, [tree]) <- makeWikiPageCommentForestWidget a b c d [e] f g h i\n+    return (widget, tree)\n+\n+makeWikiPageCommentActionWidget\n+        :: MakeCommentActionWidget\n+        -> Text\n+        -> Text\n+        -> CommentId\n+        -> CommentMods\n+        -> Handler MaxDepth\n+        -> Handler (Widget, Tree (Entity Comment))\n+makeWikiPageCommentActionWidget make_comment_action_widget project_handle target comment_id mods get_max_depth = do\n+    (user@(Entity user_id _), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id\n+    make_comment_action_widget\n+      (Entity comment_id comment)\n+      user\n+      (wikiPageCommentHandlerInfo (Just user_id) project_id project_handle target)\n+      mods\n+      get_max_depth\n+      False\n \n --------------------------------------------------------------------------------\n--- / and /reply\n-\n-getDiscussCommentR :: Text -> Text -> CommentId -> Handler Html\n-getDiscussCommentR project_handle target comment_id = do\n-    (comment_widget, comment_tree) <-\n-        makeCommentWidget\n+-- /\n+\n+getWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n+getWikiCommentR project_handle target comment_id = do\n+    (muser, Entity project_id _, _, comment) <- checkCommentPage project_handle target comment_id\n+    (widget, comment_tree) <-\n+        makeWikiPageCommentTreeWidget\n+          muser\n+          project_id\n+          project_handle\n+          target\n+          (Entity comment_id comment)\n+          def\n           getMaxDepth\n-          True\n+          False\n           mempty\n+\n+    case muser of\n+        Nothing -> return ()\n+        Just (Entity user_id _) ->\n+            runDB (userMaybeViewProjectCommentsDB user_id project_id (map entityKey (Tree.flatten comment_tree)))\n+\n+    defaultLayout $ do\n+        $(widgetFile "wiki_discussion_wrapper")\n+        toWidget $(cassiusFile "templates/comment.cassius")\n+\n+--------------------------------------------------------------------------------\n+-- /close\n+\n+getCloseWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n+getCloseWikiCommentR project_handle target comment_id = do\n+    (widget, _) <-\n+        makeWikiPageCommentActionWidget\n+          makeCloseCommentWidget\n           project_handle\n           target\n           comment_id\n-    maybeAuthId >>= \\case\n-        Nothing -> return ()\n-        Just user_id -> runYDB $ do\n-            -- this Just is ok, because makeCommentWidget will fail if the project_handle is invalid\n-            Just (Entity project_id _) <- getBy (UniqueProjectHandle project_handle)\n-            ok <- userWatchingProjectDB user_id project_id\n-            when ok $\n-                userViewCommentsDB user_id (map entityKey (Tree.flatten comment_tree))\n-    defaultLayout $(widgetFile "comment_wrapper")\n-\n-getReplyCommentR :: Text -> Text -> CommentId -> Handler Html\n-getReplyCommentR project_handle target comment_id = do\n-    void requireAuth\n-    comment_widget <- fst <$> makeCommentWidget\n-                                getMaxDepth\n-                                True\n-                                widget\n-                                project_handle\n-                                target\n-                                comment_id\n-    defaultLayout $(widgetFile "comment_wrapper")\n-  where\n-    widget = commentFormWidget "Reply" Nothing\n-\n-postReplyCommentR :: Text -> Text -> CommentId -> Handler Html\n-postReplyCommentR project_handle target comment_id = do\n-    (project, Entity _ page, _) <- checkCommentPage project_handle target comment_id\n-\n-    ((result, _), _) <- runFormPost commentReplyForm\n-\n-    case result of\n-        FormSuccess text -> do\n-            mode <- lookupPostParam "mode"\n-            processWikiComment mode (Just comment_id) text project page\n-        FormMissing      -> error "Form missing."\n-        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\\n" msgs)\n+          def\n+          getMaxDepth\n+    defaultLayout $ do\n+        $(widgetFile "wiki_discussion_wrapper")\n+        toWidget $(cassiusFile "templates/comment.cassius")\n+\n+postCloseWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n+postCloseWikiCommentR project_handle target comment_id = do\n+    (user@(Entity user_id _), (Entity project_id _), _, comment) <- checkCommentPageRequireAuth project_handle target comment_id\n+    checkWikiPageCommentActionPermission can_close project_handle (Entity comment_id comment)\n+\n+    postCloseComment\n+      user\n+      comment_id\n+      comment\n+      (wikiPageCommentHandlerInfo (Just user_id) project_id project_handle target)\n+      >>= \\case\n+        Nothing -> redirect (WikiCommentR project_handle target comment_id)\n+        Just (widget, form) -> defaultLayout $ previewWidget form "close" ($(widgetFile "wiki_discussion_wrapper"))\n \n --------------------------------------------------------------------------------\n -- /delete\n \n-getDeleteCommentR :: Text -> Text -> CommentId -> Handler Html\n-getDeleteCommentR project_handle target comment_id = do\n-    void requireAuth\n-    comment_widget <- fst <$> makeCommentWidget\n-                                getMaxDepthZero\n-                                True\n-                                widget\n-                                project_handle\n-                                target\n-                                comment_id\n-    defaultLayout $(widgetFile "comment_wrapper")\n-  where\n-    widget = [whamlet|\n-        <div>\n-            <form method=POST>\n-                <input type=submit name=mode value=Delete>\n-                <input type=submit name=mode value=Cancel>\n-    |]\n-\n-postDeleteCommentR :: Text -> Text -> CommentId -> Handler Html\n-postDeleteCommentR project_handle target comment_id =\n-    lookupPostParam "mode" >>= \\case\n-        Just "Delete" -> deleteDeleteCommentR project_handle target comment_id\n-        _             -> redirect $ DiscussCommentR project_handle target comment_id\n-\n-deleteDeleteCommentR :: Text -> Text -> CommentId -> Handler Html\n-deleteDeleteCommentR project_handle target comment_id = do\n-    user_id <- requireAuthId\n-    comment <- runYDB $ get404 comment_id\n-\n-    can_delete <- runDB $ canDeleteComment user_id (Entity comment_id comment)\n-    unless can_delete $\n-        permissionDenied "You can't delete that comment."\n-\n-    runDB $ deleteComment comment_id\n-\n-    addAlert "success" "comment deleted"\n-    redirect $ DiscussWikiR project_handle target\n+getDeleteWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n+getDeleteWikiCommentR project_handle target comment_id = do\n+    (widget, _) <-\n+        makeWikiPageCommentActionWidget\n+          makeDeleteCommentWidget\n+          project_handle\n+          target\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $ do\n+        $(widgetFile "wiki_discussion_wrapper")\n+        toWidget $(cassiusFile "templates/comment.cassius")\n+\n+postDeleteWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n+postDeleteWikiCommentR project_handle target comment_id = do\n+    (_, _, _, comment) <- checkCommentPage project_handle target comment_id\n+    checkWikiPageCommentActionPermission can_delete project_handle (Entity comment_id comment)\n+\n+    was_deleted <- postDeleteComment comment_id\n+    if was_deleted\n+        then redirect (WikiDiscussionR project_handle target)\n+        else redirect (WikiCommentR project_handle target comment_id)\n \n --------------------------------------------------------------------------------\n -- /edit\n \n-getEditCommentR :: Text -> Text -> CommentId -> Handler Html\n-getEditCommentR project_handle target comment_id = do\n-    void requireAuth\n-    comment <- runYDB $ get404 comment_id\n-    comment_widget <- fst <$> makeCommentWidget\n-                                getMaxDepthZero\n-                                True\n-                                (commentEditFormWidget $ commentText comment)\n-                                project_handle\n-                                target\n-                                comment_id\n-    defaultLayout $(widgetFile "comment_wrapper")\n-\n-postEditCommentR :: Text -> Text -> CommentId -> Handler Html\n-postEditCommentR project_handle target comment_id = do\n-    void $ checkCommentPage project_handle target comment_id\n-    ((result, _), _) <- runFormPost $ commentEditForm ""\n-    case result of\n-        FormSuccess new_text -> lookupPostParam "mode" >>= \\case\n-            Just "post"    -> postEdit new_text\n-            Just "preview" -> previewEdit new_text\n-            m              -> error $ "Error: unrecognized mode (" ++ show m ++ ")"\n-        FormMissing -> error "Form missing."\n-        FormFailure msgs -> error $ "Error submitting form: " ++ T.unpack (T.intercalate "\\n" msgs)\n-  where\n-    previewEdit :: Markdown -> Handler Html\n-    previewEdit new_text = do\n-        (form, _) <- generateFormPost $ commentEditForm new_text\n-        comment_widget <- previewWidget form "post" . fst <$>\n-            makeCommentWidgetMod\n-              mods\n-              getMaxDepthZero\n-              False\n-              mempty\n-              project_handle\n-              target\n-              comment_id\n-        defaultLayout $(widgetFile "comment_wrapper")\n-      where\n-        mods :: CommentMods\n-        mods = def\n-            { mod_comment = \\c -> c { commentText = new_text }\n-            -- Since an edit removes a flagging, don't show the flagged markup in preview.\n-            , mod_flag_map = M.delete comment_id\n-            }\n-\n-    postEdit :: Markdown -> Handler Html\n-    postEdit new_text = do\n-        user_id <- requireAuthId\n-        (_, _, comment) <- checkCommentPage project_handle target comment_id\n-\n-        unless (canEditComment user_id comment) $\n-            permissionDenied "You can't edit that comment."\n-\n-        runSYDB $ editComment comment_id new_text\n-\n-        addAlert "success" "posted new edit"\n-        redirect $ DiscussCommentR project_handle target comment_id\n+getEditWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n+getEditWikiCommentR project_handle target comment_id = do\n+    (widget, _) <-\n+        makeWikiPageCommentActionWidget\n+          makeEditCommentWidget\n+          project_handle\n+          target\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $ do\n+        $(widgetFile "wiki_discussion_wrapper")\n+        toWidget $(cassiusFile "templates/comment.cassius")\n+\n+postEditWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n+postEditWikiCommentR project_handle target comment_id = do\n+    (user@(Entity user_id _), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id\n+    checkWikiPageCommentActionPermission can_edit project_handle (Entity comment_id comment)\n+\n+    postEditComment\n+      user\n+      (Entity comment_id comment)\n+      (wikiPageCommentHandlerInfo (Just user_id) project_id project_handle target)\n+      >>= \\case\n+        Nothing -> redirect (WikiCommentR project_handle target comment_id)  -- Edit made.\n+        Just widget -> defaultLayout $(widgetFile "wiki_discussion_wrapper") -- Previewing edit.\n \n --------------------------------------------------------------------------------\n -- /flag\n \n-getFlagCommentR :: Text -> Text -> CommentId -> Handler Html\n-getFlagCommentR project_handle target comment_id = do\n-    void requireAuth\n-    comment_widget <- fst <$> makeCommentWidget\n-                                getMaxDepthZero\n-                                True\n-                                widget\n-                                project_handle\n-                                target\n-                                comment_id\n-    defaultLayout $(widgetFile "comment_wrapper")\n-  where\n-    widget = do\n-        (form, enctype) <- handlerToWidget $ generateFormPost (flagCommentForm Nothing Nothing)\n-        [whamlet|\n-            <form method="POST" enctype=#{enctype}>\n-                <h4>Code of Conduct Violation(s):\n-                ^{form}\n-                <input type="submit" value="preview flag message">\n-                <input type="hidden" name="mode" value="preview">\n-        |]\n-\n-postFlagCommentR :: Text -> Text -> CommentId -> Handler Html\n-postFlagCommentR project_handle target comment_id = do\n-    user_id <- requireAuthId\n-    void $ checkCommentPage project_handle target comment_id\n-    ((result, _), _) <- runFormPost (flagCommentForm Nothing Nothing)\n-    case result of\n-        -- TODO(mitchell): Change the form to just return [FlagReason], not Maybe [FlagReason]\n-        FormSuccess (Nothing, _) -> flagFailure "Please check at least one Code of Conduct violation."\n-        FormSuccess (Just [], _) -> flagFailure "Please check at least one Code of Conduct violation."\n-        FormSuccess (Just reasons, message) -> lookupPostParam "mode" >>= \\case\n-            Just "flag comment" -> postFlag user_id reasons message\n-            Just "preview"      -> previewFlag reasons message\n-            m                   -> error $ "Error: unrecognized mode (" ++ show m ++ ")"\n-        FormFailure errs -> flagFailure (T.intercalate ", " errs)\n-        _ -> flagFailure "Form missing."\n-  where\n-    postFlag :: UserId -> [FlagReason] -> Maybe Markdown -> Handler Html\n-    postFlag user_id reasons message = do\n-            permalink_route <- getUrlRender <*> pure (EditCommentR project_handle target comment_id)\n-            success <- runSYDB $ flagComment\n-                                   project_handle\n-                                   target\n-                                   comment_id\n-                                   permalink_route\n-                                   user_id\n-                                   reasons\n-                                   message\n-            if success\n-                then addAlert "success" "comment hidden and flagged for revision"\n-                else addAlert "danger" "error: another user flagged this just before you"\n-            redirect $ DiscussWikiR project_handle target\n-\n-    previewFlag :: [FlagReason] -> Maybe Markdown -> Handler Html\n-    previewFlag reasons message = do\n-        (form, _) <- generateFormPost $ flagCommentForm (Just (Just reasons)) (Just message)\n-\n-        let mods = def { mod_flag_map = M.insert comment_id (message, reasons) }\n-        unwrapped_comment_widget <- fst <$>\n-            makeCommentWidgetMod\n-              mods\n-              getMaxDepthZero\n-              True\n-              mempty\n-              project_handle\n-              target\n-              comment_id\n-\n-        let form_with_header =\n-                [whamlet|\n-                    <h4>Code of Conduct Violation(s):\n-                    ^{form}\n-                |]\n-            comment_widget = do\n-                previewWidget form_with_header "flag comment" unwrapped_comment_widget\n-                -- the CSS below styles this particular flagging submit\n-                -- button. It would be ideal to have this in a more\n-                -- generalized place so it can be reused in other flagging\n-                -- buttons and be in just one place, but this works for\n-                -- now.\n-                toWidget [cassius|\n-                    .preview-action-button[type=submit]\n-                        background : dark-red\n-                        background-image : linear-gradient(#ee2700, #bd1000)\n-                        border-color: #a5022a\n-\n-                    .preview-action-button[type=submit]:hover, .preview-action-button[type=submit]:focus, .preview-action-button[type=submit]:active\n-                        background : red\n-                        background-image : linear-gradient(#d22935, #a5022a)\n-                |]\n-        defaultLayout $(widgetFile "comment_wrapper")\n-\n-flagFailure :: Text -> Handler a\n-flagFailure msg = do\n-    addAlert "danger" msg\n-    Just route <- getCurrentRoute\n-    redirect route\n+getFlagWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n+getFlagWikiCommentR project_handle target comment_id = do\n+    (widget, _) <-\n+        makeWikiPageCommentActionWidget\n+          makeFlagCommentWidget\n+          project_handle\n+          target\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $ do\n+        $(widgetFile "wiki_discussion_wrapper")\n+        toWidget $(cassiusFile "templates/comment.cassius")\n+\n+postFlagWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n+postFlagWikiCommentR project_handle target comment_id = do\n+    (user@(Entity user_id _), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id\n+    checkWikiPageCommentActionPermission can_flag project_handle (Entity comment_id comment)\n+\n+    postFlagComment\n+      user\n+      (Entity comment_id comment)\n+      (wikiPageCommentHandlerInfo (Just user_id) project_id project_handle target)\n+      >>= \\case\n+        Nothing -> redirect (WikiDiscussionR project_handle target)\n+        Just widget -> defaultLayout $(widgetFile "wiki_discussion_wrapper")\n \n --------------------------------------------------------------------------------\n--- /moderate\n+-- /moderate TODO: rename to /approve\n \n getApproveWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n getApproveWikiCommentR project_handle target comment_id = do\n-    void $ sanityCheckApprove project_handle\n-    comment_widget <- fst <$> makeCommentWidget\n-                                getMaxDepth\n-                                True\n-                                widget\n-                                project_handle\n-                                target\n-                                comment_id\n-    defaultLayout $(widgetFile "comment_wrapper")\n-  where\n-    widget = [whamlet|\n-        <form method="POST">\n-            <input type=submit value="approve post">\n-    |]\n+    (widget, _) <-\n+        makeWikiPageCommentActionWidget\n+          makeApproveCommentWidget\n+          project_handle\n+          target\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $ do\n+        $(widgetFile "wiki_discussion_wrapper")\n+        toWidget $(cassiusFile "templates/comment.cassius")\n \n postApproveWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n postApproveWikiCommentR project_handle target comment_id = do\n-    (_, _, comment) <- checkCommentPage project_handle target comment_id\n-    user_id <- sanityCheckApprove project_handle\n-\n-    runSDB $ approveComment user_id comment_id comment\n-    addAlert "success" "comment approved"\n-    redirect $ DiscussCommentR project_handle target comment_id\n+    (Entity user_id _, _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id\n+    checkWikiPageCommentActionPermission can_approve project_handle (Entity comment_id comment)\n \n-sanityCheckApprove :: Text -> Handler UserId\n-sanityCheckApprove project_handle = do\n-    user_id <- requireAuthId\n-    requireModerator "You must be a moderator to approve posts." project_handle user_id\n-    return user_id\n+    postApproveComment user_id comment_id comment\n+    redirect (WikiCommentR project_handle target comment_id)\n \n --------------------------------------------------------------------------------\n--- /close and /retract\n+-- /reply\n \n-getRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n-getRetractWikiCommentR project_handle target comment_id = do\n-    -- This function calls checkCommentPage twice: once in retractSanityCheck, and another\n-    -- time in makeCommentWidget. Maybe it should be taken out of makeCommentWidget,\n-    -- and the handlers should be in charge of calling it?\n-    retractSanityCheck project_handle target comment_id\n-    comment_widget <- fst <$> makeCommentWidget\n-                                getMaxDepth\n-                                True\n-                                (commentRetractFormWidget Nothing)\n-                                project_handle\n-                                target\n-                                comment_id\n-    defaultLayout $(widgetFile "comment_wrapper")\n-\n-retractSanityCheck :: Text -> Text -> CommentId -> Handler ()\n-retractSanityCheck project_handle target comment_id = do\n-    user_id <- requireAuthId\n-    (_, _, comment) <- checkCommentPage project_handle target comment_id\n-    when (commentUser comment /= user_id) $\n-        permissionDenied "You can only retract your own comments."\n-\n-getCloseWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n-getCloseWikiCommentR project_handle target comment_id = do\n-    closeSanityCheck project_handle target comment_id\n-    comment_widget <- fst <$> makeCommentWidget\n-                                getMaxDepth\n-                                True\n-                                (commentCloseFormWidget Nothing)\n-                                project_handle\n-                                target\n-                                comment_id\n-    defaultLayout $(widgetFile "comment_wrapper")\n-\n--- Same signature as retractSanityCheck, for use in postClosureWikiComment\n-closeSanityCheck :: Text -> Text -> CommentId -> Handler ()\n-closeSanityCheck _ _ _ = do\n-    user <- entityVal <$> requireAuth\n-\n-    -- TODO: what should this be?\n-    -- Aaron says: I think we should allow established to mark as closed,\n-    -- but only *affiliated* OR the original poster should do so in one step,\n-    -- otherwise, the marking of closed should require *moderator* confirmation…\n-    -- We should also have a re-open function.\n-    -- There are now comments discussing these things on the site.\n-    unless (isEstablished user) $\n-        permissionDenied "You must be an established user to close a conversation."\n-\n-postRetractWikiCommentR, postCloseWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n-postRetractWikiCommentR = postClosureWikiComment retractSanityCheck commentRetractForm newRetractedCommentClosure "retract"\n-postCloseWikiCommentR   = postClosureWikiComment closeSanityCheck   commentCloseForm   newClosedCommentClosure    "close"\n-\n--- | POST handler for either closing or retracting a comment, which are very similar.\n-postClosureWikiComment :: (Text -> Text -> CommentId -> Handler ())\n-                       -> (Maybe Markdown -> Form Markdown)\n-                       -> (UserId -> Markdown -> CommentId -> Handler CommentClosure)\n-                       -> Text\n-                       -> Text\n-                       -> Text\n-                       -> CommentId\n-                       -> Handler Html\n-postClosureWikiComment sanity_check make_closure_form make_new_comment_closure action project_handle target comment_id = do\n-    sanity_check project_handle target comment_id\n-    ((result, _), _) <- runFormPost $ make_closure_form Nothing\n-    case result of\n-        FormSuccess reason -> do\n-            user_id <- requireAuthId\n-            new_comment_closure <- make_new_comment_closure user_id reason comment_id\n-            lookupPostParam "mode" >>= \\case\n-                Just "preview" -> do\n-                    (form, _) <- generateFormPost $ make_closure_form (Just reason)\n-                    comment_widget <- previewWidget form action . fst <$>\n-                                          makeCommentWidgetMod\n-                                            (def { mod_closure_map = M.insert comment_id new_comment_closure })\n-                                            getMaxDepthZero\n-                                            False\n-                                            mempty -- TODO(mitchell): is this right?\n-                                            project_handle\n-                                            target\n-                                            comment_id\n-                    defaultLayout $(widgetFile "comment_wrapper")\n-                Just mode | mode == action -> do\n-                    runDB $ insert_ new_comment_closure\n-                    redirect $ DiscussCommentR project_handle target comment_id\n-                mode -> error $ "Error: unrecognized mode (" ++ show mode ++ ")"\n-        _ -> error "Error when submitting form."\n+getReplyWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n+getReplyWikiCommentR project_handle target comment_id = do\n+    (widget, _) <-\n+        makeWikiPageCommentActionWidget\n+          makeReplyCommentWidget\n+          project_handle\n+          target\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $ do\n+        $(widgetFile "wiki_discussion_wrapper")\n+        toWidget $(cassiusFile "templates/comment.cassius")\n+\n+postReplyWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n+postReplyWikiCommentR project_handle target parent_id = do\n+    (user, _, Entity _ page, parent) <- checkCommentPageRequireAuth project_handle target parent_id\n+    checkWikiPageCommentActionPermission can_reply project_handle (Entity parent_id parent)\n+\n+    postNewComment\n+      (Just parent_id)\n+      user\n+      (wikiPageDiscussion page)\n+      (makeProjectCommentActionPermissions project_handle) >>= \\case\n+        Left _ -> redirect (WikiCommentR project_handle target parent_id)\n+        Right (widget, form) -> defaultLayout $ previewWidget form "post" ($(widgetFile "wiki_discussion_wrapper"))\n \n --------------------------------------------------------------------------------\n -- /rethread\n \n getRethreadWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n-getRethreadWikiCommentR _ _ _ = do\n-    (form, _) <- generateFormPost rethreadForm\n-    defaultLayout $(widgetFile "rethread")\n+getRethreadWikiCommentR project_handle target comment_id = do\n+    (widget, _) <-\n+        makeWikiPageCommentActionWidget\n+          makeRethreadCommentWidget\n+          project_handle\n+          target\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $ do\n+        $(widgetFile "wiki_discussion_wrapper")\n+        toWidget $(cassiusFile "templates/comment.cassius")\n \n postRethreadWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n postRethreadWikiCommentR project_handle target comment_id = do\n-    -- TODO (0): AVOID CYCLES\n-\n-    (Entity project_id _, _, comment) <- checkCommentPage project_handle target comment_id\n-\n-    user_id <- requireAuthId\n-    ok <- runDB $ isProjectModerator' user_id project_id\n-    unless ok $\n-        permissionDenied "You must be a moderator to rethread"\n-\n-    ((result, _), _) <- runFormPost rethreadForm\n-\n-    case result of\n-        FormSuccess (new_parent_url, reason) -> do\n-            app <- getYesod\n-            let splitPath  = drop 1 . T.splitOn "/"\n-                stripQuery = fst . T.break (== '?')\n-                stripRoot  = fromMaybe new_parent_url . T.stripPrefix (appRoot $ settings app)\n-                url        = splitPath $ stripQuery $ stripRoot new_parent_url\n-\n-            (new_parent_id, new_discussion_id) <- case parseRoute (url, []) of\n-                Just (DiscussCommentR new_project_handle new_target new_parent_id) -> do\n-                    new_discussion_id <- getNewDiscussionId user_id project_id new_project_handle new_target\n-                    return (Just new_parent_id, new_discussion_id)\n-\n-                Just (DiscussWikiR new_project_handle new_target) -> do\n-                    new_discussion_id <- getNewDiscussionId user_id project_id new_project_handle new_target\n-                    return (Nothing, new_discussion_id)\n-\n-                Nothing -> error "failed to parse URL"\n-\n-                _ -> error "could not find discussion for that URL"\n-\n-            let old_parent_id = commentParent comment\n-            when (new_parent_id == old_parent_id && new_discussion_id == commentDiscussion comment) $\n-                error "trying to move comment to its current location"\n-\n-            new_parent_depth <- maybe (return $ -1) getCommentDepth404 new_parent_id\n-            old_parent_depth <- maybe (return $ -1) getCommentDepth404 old_parent_id\n+    (Entity user_id _, _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id\n+    checkWikiPageCommentActionPermission can_rethread project_handle (Entity comment_id comment)\n+    postRethreadComment user_id comment_id comment\n \n-            let depth_offset = old_parent_depth - new_parent_depth\n-\n-            mode <- lookupPostParam "mode"\n-            let action :: Text = "rethread"\n-            case mode of\n-                Just "preview" -> error "no preview for rethreads yet" -- TODO\n-\n-                Just action' | action' == action -> do\n-                    now <- liftIO getCurrentTime\n-\n-                    runDB $ do\n-                        descendants <- getCommentDescendantsIds comment_id\n-\n-                        let comments = comment_id : descendants\n-\n-                        rethread_id <- insert $ Rethread now user_id comment_id reason\n-\n-                        new_comment_ids <- rethreadComments rethread_id depth_offset new_parent_id new_discussion_id comments\n-\n-                        delete $\n-                            from $ \\ca ->\n-                            where_ $ ca ^. CommentAncestorComment `in_` valList comments\n-\n-                        forM_ new_comment_ids $ \\ new_comment_id -> do\n-                            insertSelect $\n-                                from $ \\ (c `InnerJoin` ca) -> do\n-                                on_ $ c ^. CommentParent ==. just (ca ^. CommentAncestorComment)\n-                                where_ $ c ^. CommentId ==. val new_comment_id\n-                                return $ CommentAncestor <# val new_comment_id <&> (ca ^. CommentAncestorAncestor)\n-\n-                            [Value maybe_new_parent_id] <-\n-                                select $\n-                                    from $ \\ c -> do\n-                                    where_ $ c ^. CommentId ==. val new_comment_id\n-                                    return (c ^. CommentParent)\n-\n-                            maybe (return ()) (insert_ . CommentAncestor new_comment_id) maybe_new_parent_id\n+--------------------------------------------------------------------------------\n+-- /retract\n \n-                        when (new_discussion_id /= commentDiscussion comment) $\n-                            update $ \\c -> do\n-                                where_ $ c ^. CommentId `in_` valList descendants\n-                                set c [ CommentDiscussion =. val new_discussion_id ]\n+getRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n+getRetractWikiCommentR project_handle target comment_id = do\n+    (widget, _) <-\n+        makeWikiPageCommentActionWidget\n+          makeRetractCommentWidget\n+          project_handle\n+          target\n+          comment_id\n+          def\n+          getMaxDepth\n+    defaultLayout $ do\n+        $(widgetFile "wiki_discussion_wrapper")\n+        toWidget $(cassiusFile "templates/comment.cassius")\n+\n+postRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n+postRetractWikiCommentR project_handle target comment_id = do\n+    (user@(Entity user_id _), Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id\n+    checkWikiPageCommentActionPermission can_retract project_handle (Entity comment_id comment)\n+\n+    postRetractComment\n+      user\n+      comment_id\n+      comment\n+      (wikiPageCommentHandlerInfo (Just user_id) project_id project_handle target)\n+      >>= \\case\n+        Nothing -> redirect (WikiCommentR project_handle target comment_id)\n+        Just (widget, form) -> defaultLayout $ previewWidget form "retract" ($(widgetFile "wiki_discussion_wrapper"))\n \n-                    redirect new_parent_url\n+--------------------------------------------------------------------------------\n+-- /tags\n \n-                m -> error $ "Error: unrecognized mode (" ++ show m ++ ")"\n-        _ -> error "Error when submitting form."\n-  where\n-    getNewDiscussionId :: UserId -> ProjectId -> Text -> Text -> Handler DiscussionId\n-    getNewDiscussionId user_id project_id new_project_handle new_target = do\n-        Entity new_project_id _ <- getByErr "could not find project" $ UniqueProjectHandle new_project_handle\n-        when (new_project_id /= project_id) $\n-            requireModerator "You must be a moderator to rethread." new_project_handle user_id\n-        maybe (error "could not find new page") (wikiPageDiscussion . entityVal) <$>\n-            runDB (getBy $ UniqueWikiTarget new_project_id new_target)\n+getWikiCommentTagsR :: Text -> Text -> CommentId -> Handler Html\n+getWikiCommentTagsR _ _ = getCommentTags\n \n --------------------------------------------------------------------------------\n--- /tags/*\n+-- /tag/#TagId\n \n-getCommentTagsR :: Text -> Text -> CommentId -> Handler Html\n-getCommentTagsR project_handle target comment_id = do\n-    void $ checkCommentPage project_handle target comment_id\n+getWikiCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html\n+getWikiCommentTagR _ _ = getCommentTagR\n \n-    comment_tags <- map entityVal <$> runDB (getCommentTags comment_id)\n-\n-    let tag_ids = S.toList . S.fromList $ map commentTagTag comment_tags\n-    tag_map <- fmap entitiesMap . runYDB $\n-        select $\n-            from $ \\tag -> do\n-            where_ (tag ^. TagId `in_` valList tag_ids)\n-            return tag\n+postWikiCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html\n+postWikiCommentTagR project_handle target comment_id tag_id = do\n+    postCommentTag comment_id tag_id\n+    redirect (WikiCommentTagR project_handle target comment_id tag_id)\n \n-    renderTags =<< buildAnnotatedTags tag_map (CommentTagR project_handle target comment_id) comment_tags\n-  where\n-    renderTags tags = defaultLayout $(widgetFile "tags")\n+--------------------------------------------------------------------------------\n+-- /tag/apply, /tag/create\n \n-getCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html\n-getCommentTagR project_handle target comment_id tag_id = do\n-    void $  checkCommentPage project_handle target comment_id\n+postWikiCommentApplyTagR, postWikiCommentCreateTagR :: Text -> Text -> CommentId -> Handler Html\n+postWikiCommentApplyTagR  = applyOrCreate postCommentApplyTag\n+postWikiCommentCreateTagR = applyOrCreate postCommentCreateTag\n \n-    comment_tags <- map entityVal <$> runDB (\n-        select $\n-            from $ \\comment_tag -> do\n-            where_ (comment_tag ^. CommentTagComment ==. val comment_id &&.\n-                    comment_tag ^. CommentTagTag ==. val tag_id)\n-            return comment_tag)\n-\n-    let tag_ids = S.toList . S.fromList $ map commentTagTag comment_tags\n-    tag_map <- fmap entitiesMap $ runDB $ select $ from $ \\ tag -> do\n-        where_ $ tag ^. TagId `in_` valList tag_ids\n-        return tag\n-\n-    annotated_tags <- buildAnnotatedTags tag_map (CommentTagR project_handle target comment_id) comment_tags\n-\n-    case annotated_tags of\n-        [] -> error "That tag has not been applied to this comment."\n-        [tag] -> renderTag tag\n-        _ -> error "This should never happen."\n-  where\n-    renderTag (AnnotatedTag tag _ _ user_votes) = do\n-        let tag_name = tagName $ entityVal tag\n-        defaultLayout $(widgetFile "tag")\n-\n-postCommentTagR :: Text -> Text -> CommentId -> TagId -> Handler Html\n-postCommentTagR project_handle target comment_id tag_id = do\n-    user_id <- requireAuthId\n-    void $  checkCommentPage project_handle target comment_id\n-\n-    direction <- lookupPostParam "direction"\n-\n-    let delta = case T.unpack <$> direction of\n-            Just "+" -> 1\n-            Just "-" -> -1\n-            Just "\\215" -> -1\n-            Nothing -> error "direction unset"\n-            Just str -> error $ "unrecognized direction: " ++ str\n-\n-    runDB $ do\n-        maybe_comment_tag_entity <- getBy $ UniqueCommentTag comment_id tag_id user_id\n-        case maybe_comment_tag_entity of\n-            Nothing -> void $ insert $ CommentTag comment_id tag_id user_id delta\n-            Just (Entity comment_tag_id comment_tag) -> case commentTagCount comment_tag + delta of\n-                0 -> delete $ from $ \\ ct -> where_ $ ct ^. CommentTagId ==. val comment_tag_id\n-                x -> void $ update $ \\ ct -> do\n-                    set ct [ CommentTagCount =. val x ]\n-                    where_ $ ct ^. CommentTagId ==. val comment_tag_id\n-\n-    setUltDestReferer\n-    redirectUltDest $ CommentTagR project_handle target comment_id tag_id\n-\n-getNewCommentTagR :: Text -> Text -> CommentId -> Handler Html\n-getNewCommentTagR project_handle target comment_id = do\n-    void . runYDB $ get404 comment_id\n-\n-    user <- entityVal <$> requireAuth\n-\n-    unless (isEstablished user)\n-        (permissionDenied "You must be an established user to add tags")\n-\n-    (Entity project_id _, _, _) <- checkCommentPage project_handle target comment_id\n-\n-    comment_tags <- fmap (map entityVal) $ runDB $ select $ from $ \\ comment_tag -> do\n-        where_ $ comment_tag ^. CommentTagComment ==. val comment_id\n-        return comment_tag\n-\n-    tag_map <- fmap entitiesMap $ runDB $ select $ from $ \\ tag -> do\n-        where_ $ tag ^. TagId `in_` valList (S.toList $ S.fromList $ map commentTagTag comment_tags)\n-        return tag\n-\n-    tags <- annotateCommentTags tag_map project_handle target comment_id comment_tags\n-\n-    (project_tags, other_tags) <- runDB $ getProjectTagList project_id\n-\n-    let filter_tags = filter (\\(Entity t _) -> not $ M.member t tag_map)\n-    (apply_form, _) <- generateFormPost $ newCommentTagForm (filter_tags project_tags) (filter_tags other_tags)\n-    (create_form, _) <- generateFormPost $ createCommentTagForm\n-\n-    defaultLayout $(widgetFile "new_comment_tag")\n-\n-postCreateNewCommentTagR, postApplyNewCommentTagR :: Text -> Text -> CommentId -> Handler Html\n-postCreateNewCommentTagR = postNewCommentTagR True\n-postApplyNewCommentTagR  = postNewCommentTagR False\n-\n-postNewCommentTagR :: Bool -> Text -> Text -> CommentId -> Handler Html\n-postNewCommentTagR create_tag project_handle target comment_id = do\n-    Entity user_id user <- requireAuth\n-\n-    unless (isEstablished user)\n-        (permissionDenied "You must be an established user to add tags")\n-\n-    (Entity project_id _, _, _) <- checkCommentPage project_handle target comment_id\n-\n-    let formFailure es = error $ T.unpack $ "form submission failed: " <> T.intercalate "; " es\n-\n-    if create_tag\n-        then do\n-            ((result_create, _), _) <- runFormPost $ createCommentTagForm\n-            case result_create of\n-                FormSuccess (tag_name) -> do\n-                    msuccess <- runDB $ do\n-                        maybe_tag <- getBy $ UniqueTag tag_name\n-                        case maybe_tag of\n-                            Nothing -> do\n-                                tag_id <- insert $ Tag tag_name\n-                                void $ insert $ CommentTag comment_id tag_id user_id 1\n-                            Just _ -> do\n-                                return ()\n-                        return maybe_tag\n-                    if (isJust $ msuccess) then do\n-                        addAlert "danger" "that tag already exists"\n-                        redirectUltDest $ NewCommentTagR project_handle target comment_id\n-                        else do\n-                            redirectUltDest $ DiscussCommentR project_handle target comment_id\n-                FormMissing -> error "form missing"\n-                FormFailure es -> formFailure es\n-        else do\n-            comment_tags <- fmap (map entityVal) $ runDB $ select $ from $ \\ comment_tag -> do\n-                where_ $ comment_tag ^. CommentTagComment ==. val comment_id\n-                return comment_tag\n-\n-            tag_map <- fmap entitiesMap $ runDB $ select $ from $ \\ tag -> do\n-                where_ $ tag ^. TagId `in_` valList (S.toList $ S.fromList $ map commentTagTag comment_tags)\n-                return tag\n-            let filter_tags = filter (\\(Entity t _) -> not $ M.member t tag_map)\n-            (project_tags, other_tags) <- runDB $ getProjectTagList project_id\n-            ((result_apply, _), _) <- runFormPost $ newCommentTagForm (filter_tags project_tags) (filter_tags other_tags)\n-            case result_apply of\n-                FormSuccess (mproject_tag_ids, mother_tag_ids) -> do\n-                    let project_tag_ids = fromMaybe [] mproject_tag_ids\n-                    let other_tag_ids = fromMaybe [] mother_tag_ids\n-                    runYDB $ do\n-                        let tag_ids = project_tag_ids <> other_tag_ids\n-                        valid_tags <- select $ from $ \\tag -> do\n-                            where_ ( tag ^. TagId `in_` valList tag_ids )\n-                            return tag\n-                        if (null valid_tags)\n-                            then\n-                                permissionDenied "error: invalid tag id"\n-                            else\n-                                void $ insertMany $ fmap (\\(Entity tag_id _) -> CommentTag comment_id tag_id user_id 1) valid_tags\n-                        -- case maybe_tag of\n-                        --    Nothing -> permissionDenied "tag does not exist"\n-                        --    Just _ -> void $ insert $ CommentTag comment_id tag_id user_id 1\n-                    redirectUltDest $ DiscussCommentR project_handle target comment_id\n-                FormMissing -> error "form missing"\n-                FormFailure es -> formFailure (es <> [T.pack " apply"])\n-\n--- Some additional helpers. These sort of belong in View, but that would cause a\n--- circular dependency.\n-\n--- | Data type used in makeCommentWidgetMod, containing modifications to comment-action-related\n--- data structures.\n-data CommentMods = CommentMods\n-    { mod_comment          :: Comment          -> Comment\n-    , mod_earlier_closures :: [CommentClosure] -> [CommentClosure]\n-    , mod_user_map         :: UserMap          -> UserMap\n-    , mod_closure_map      :: ClosureMap       -> ClosureMap\n-    , mod_ticket_map       :: TicketMap        -> TicketMap\n-    , mod_flag_map         :: FlagMap          -> FlagMap\n-    , mod_tag_map          :: TagMap           -> TagMap\n-    }\n-\n-instance Default CommentMods where\n-    def = CommentMods id id id id id id id\n-\n--- | Helper method to create a Widget for a comment action (/, /reply, /moderate, etc).\n---\n--- Returns:\n---    a widget, so that the widget can possibly be put in a preview (for some POST handlers).\n---    a comment tree, so that additional actions may be taken on the comments actually\n---    shown (such as marking them all as viewed)\n-makeCommentWidget :: Handler Int    -- ^ Max depth getter.\n-                  -> Bool           -- ^ Show actions?\n-                  -> Widget         -- ^ Widget to display under root comment.\n-                  -> Text           -- ^ Project handle.\n-                  -> Text           -- ^ Target.\n-                  -> CommentId      -- ^ Root comment id.\n-                  -> Handler (Widget, Tree (Entity Comment))\n-makeCommentWidget = makeCommentWidgetMod def\n-\n--- | Like @makeCommentWidget@, but includes modifications to the datastructures grabbed from\n--- the database. This is used for showing previews of comment trees, where changes are not\n--- saved yet.\n-makeCommentWidgetMod :: CommentMods    -- ^ Comment structure modifications.\n-                     -> Handler Int    -- ^ Max depth getter.\n-                     -> Bool           -- ^ Is preview_\n-                     -> Widget         -- ^ Widget to display under root comment.\n-                     -> Text           -- ^ Project handle.\n-                     -> Text           -- ^ Target.\n-                     -> CommentId      -- ^ Root comment id.\n-                     -> Handler (Widget, Tree (Entity Comment))\n-makeCommentWidgetMod CommentMods{..} get_max_depth show_actions form_under_root_comment project_handle target comment_id = do\n-    (Entity project_id _, _, root) <-\n-        -- TODO(mitchell)\n-        -- (_3 %~ mod_comment) <$> checkCommentPage project_handle target comment_id\n-        (\\(a,b,c) -> (a,b,mod_comment c)) <$> checkCommentPage project_handle target comment_id\n-\n-    mviewer_id <- maybeAuthId\n-    (rest, user_map, earlier_closures, closure_map, ticket_map, flag_map, tag_map) <- runDB $ do\n-        rest <- getCommentDescendants mviewer_id project_id comment_id\n-\n-        let all_comments    = (Entity comment_id root):rest\n-            all_comment_ids = map entityKey all_comments\n-\n-        earlier_closures <- getAncestorClosures comment_id\n-        user_map         <- entitiesMap <$> fetchUsersInDB (S.toList $ getCommentsUsers all_comments)\n-        closure_map      <- makeClosureMap all_comment_ids\n-        ticket_map       <- makeTicketMap  all_comment_ids\n-        flag_map         <- makeFlagMap    all_comment_ids\n-        tag_map          <- entitiesMap <$> getAllTags\n-\n-        return (rest, user_map, earlier_closures, closure_map, ticket_map, flag_map, tag_map)\n-\n-    user_map_with_viewer <- (maybe id (\\(Entity viewer_id viewer) -> M.insert viewer_id viewer))\n-        <$> maybeAuth\n-        <*> pure user_map\n-\n-    max_depth <- get_max_depth\n-    let comment_tree = sortTreeBy orderingNewestFirst (buildCommentTree (Entity comment_id root, rest))\n-        comment_tree_widget =\n-            commentTreeWidget\n-                form_under_root_comment\n-                comment_tree\n-                (mod_earlier_closures earlier_closures)\n-                (mod_user_map         user_map_with_viewer)\n-                (mod_closure_map      closure_map)\n-                (mod_ticket_map       ticket_map)\n-                (mod_flag_map         flag_map)\n-                (mod_tag_map          tag_map)\n-                project_handle\n-                target\n-                show_actions\n-                max_depth\n-                0\n-    return (comment_tree_widget, comment_tree)\n+applyOrCreate :: (CommentId -> Handler ()) -> Text -> Text -> CommentId -> Handler Html\n+applyOrCreate action project_handle target comment_id = do\n+    action comment_id\n+    redirect (WikiCommentR project_handle target comment_id)\n \n --------------------------------------------------------------------------------\n--- Experimental - /c/#CommentId\n+-- /tag/new\n \n-getCommentDirectLinkR :: CommentId -> Handler Html\n-getCommentDirectLinkR comment_id = runDB (getCommentPageEntity' comment_id) >>= \\case\n-    -- comment not on a wiki page? right now, there's nowhere else to check\n-    -- TODO: fixme once discussions are expanded\n-    Nothing -> notFound\n-    Just (Entity _ page) -> do\n-        project <- runYDB $ get404 (wikiPageProject page)\n-        redirect (DiscussCommentR (projectHandle project) (wikiPageTarget page) comment_id)\n+getWikiCommentAddTagR :: Text -> Text -> CommentId -> Handler Html\n+getWikiCommentAddTagR project_handle target comment_id = do\n+    (Entity user_id _, Entity project_id _, _, comment) <- checkCommentPageRequireAuth project_handle target comment_id\n+    ok <- can_add_tag <$> makeProjectCommentActionPermissions project_handle (Entity comment_id comment)\n+    unless ok (permissionDenied "You don't have permission to view this page.")\n+    getProjectCommentAddTag comment_id project_id user_id\n \n --------------------------------------------------------------------------------\n -- DEPRECATED\n@@ -960,4 +428,4 @@ getCommentDirectLinkR comment_id = runDB (getCommentPageEntity' comment_id) >>=\n -- This is just because we used to have "/comment/#" with that longer URL,\n -- and this keeps any permalinks from breaking\n getOldDiscussCommentR :: Text -> Text -> CommentId -> Handler Html\n-getOldDiscussCommentR project_handle target comment_id = redirect $ DiscussCommentR project_handle target comment_id\n+getOldDiscussCommentR project_handle target comment_id = redirect $ WikiCommentR project_handle target comment_id\ndiff --git a/Model.hs b/Model.hs\nindex 127b83d..0451fbe 100644\n--- a/Model.hs\n+++ b/Model.hs\n@@ -2,27 +2,27 @@\n \n module Model where\n \n-import Model.Comment.Internal     (ClosureType, FlagReason)\n-import Model.Currency             (Milray)\n-import Model.Established.Internal (Established(..))\n-import Model.Markdown.Diff        (MarkdownDiff)\n-import Model.Message.Internal     (MessageType, MessageDelivery)\n-import Model.Permission.Internal  (PermissionLevel)\n-import Model.Role.Internal        (Role)\n-import Model.Settings.Internal    (UserSettingName)\n-import Model.ViewType.Internal    (ViewType)\n-\n-import Control.Exception          (Exception)\n-import Data.Int                   (Int64)\n-import Data.Function              (on)\n-import Data.Text                  (Text)\n-import Data.Time.Clock            (UTCTime)\n-import Data.Typeable              (Typeable)\n+import Model.Comment.Internal      (ClosureType, FlagReason)\n+import Model.Currency              (Milray)\n+import Model.Established.Internal  (Established(..))\n+import Model.Markdown.Diff         (MarkdownDiff)\n+import Model.Notification.Internal (NotificationType, NotificationDelivery)\n+import Model.Permission.Internal   (PermissionLevel)\n+import Model.Role.Internal         (Role)\n+import Model.Settings.Internal     (UserSettingName)\n+import Model.ViewType.Internal     (ViewType)\n+\n+import Control.Exception           (Exception)\n+import Data.Int                    (Int64)\n+import Data.Function               (on)\n+import Data.Text                   (Text)\n+import Data.Time.Clock             (UTCTime)\n+import Data.Typeable               (Typeable)\n import Database.Persist.Quasi\n import Prelude\n import Yesod\n-import Yesod.Auth.HashDB          (HashDBUser (..))\n-import Yesod.Markdown             (Markdown)\n+import Yesod.Auth.HashDB           (HashDBUser (..))\n+import Yesod.Markdown              (Markdown)\n \n -- You can define all of your database entities in the entities file.\n -- You can find more information on persistent and how to declare entities\ndiff --git a/Model/AnnotatedTag.hs b/Model/AnnotatedTag.hs\ndeleted file mode 100644\nindex ff0faa5..0000000\n--- a/Model/AnnotatedTag.hs\n+++ /dev/null\n@@ -1,79 +0,0 @@\n-\n-module Model.AnnotatedTag where\n-\n-import Import\n-\n-import qualified Data.Map as M\n-import qualified Data.Set as S\n-import qualified Data.List as L\n-\n-import Data.List (sortBy)\n-\n-import Text.Printf\n-\n-data AnnotatedTag = AnnotatedTag\n-    { atTag       :: Entity Tag\n-    , atUrl       :: Route App\n-    , atColor     :: Color\n-    , atUserVotes :: [(Entity User, Int)]\n-    }\n-\n-atName :: AnnotatedTag -> Text\n-atName = tagName . entityVal . atTag\n-\n-{- Scoring for voting on tags is something not currently presented\n-- on the site. We've discussed changing it. I (Aaron) prefer a 6-point\n-- range voting just like we proposed for the Bylaws instead of mimicking\n-- the pledge formula here. Final decisions haven't been made yet -}\n-\n-atScore :: AnnotatedTag -> Double\n-atScore = sum . map (\\ (_, x) -> if x == 0 then 0 else fromIntegral (signum x) * logBase 2 (1 + fromIntegral (abs x) :: Double)) . atUserVotes\n-\n-atUserScore :: AnnotatedTag -> UserId -> Maybe Int\n-atUserScore at user_id = fmap snd $ L.find ((== user_id) . entityKey . fst) $ atUserVotes at\n-\n-atScoreString :: AnnotatedTag -> String\n-atScoreString = printf "%.1f" . atScore\n-\n-buildAnnotatedTags :: Map TagId Tag -> (TagId -> Route App) -> [CommentTag] -> Handler [AnnotatedTag]\n-buildAnnotatedTags tag_map tagUrl comment_tags = do\n-    let tags :: [(TagId, (UserId, Int))]\n-        tags = map (commentTagTag &&& (commentTagUser &&& commentTagCount)) comment_tags\n-\n-    user_map <- fmap entitiesMap $ runDB $\n-        select $\n-        from $ \\user -> do\n-        where_ $ user ^. UserId `in_` valList (S.toList . S.fromList $ map (fst . snd) tags)\n-        return user\n-\n-    tag_colors <- fmap (M.mapKeysMonotonic Key) $ cached $ fmap M.fromList $ do\n-        maybe_user_id <- maybeAuthId\n-        case maybe_user_id of\n-            Nothing -> do\n-                colors <- runDB $ select $ from return\n-                return $ map ((unKey . defaultTagColorTag &&& Color . defaultTagColorColor) . entityVal) colors\n-\n-            Just user_id -> do\n-                colors <- runDB $\n-                    select $\n-                    from $ \\ tag_color -> do\n-                    where_ $ tag_color ^. TagColorUser ==. val user_id\n-                    return tag_color\n-\n-                return $ map ((unKey . tagColorTag &&& Color . tagColorColor) . entityVal) colors\n-\n-    let merged_tags = M.toList $ M.fromListWith (++) $ map (second return) tags\n-\n-    annotated_tags <- forM merged_tags $ \\ (tag_id, user_votes) -> do\n-        let tag = Entity tag_id $ tag_map M.! tag_id\n-            user_votes' :: [(Entity User, Int)]\n-            user_votes' = map (first (uncurry Entity . (id &&& (user_map M.!)))) user_votes\n-            sorted_user_votes :: [(Entity User, Int)]\n-            sorted_user_votes = sortBy (compare `on` (userName . entityVal . fst)) user_votes'\n-\n-        return $ AnnotatedTag tag (tagUrl tag_id) (M.findWithDefault 0x77AADD tag_id tag_colors) sorted_user_votes\n-\n-    return $ sortBy (compare `on` atScore) annotated_tags\n-\n-annotateCommentTags :: Map TagId Tag -> Text -> Text -> CommentId -> [CommentTag] -> Handler [AnnotatedTag]\n-annotateCommentTags tag_map project_handle target comment_id = buildAnnotatedTags tag_map (CommentTagR project_handle target comment_id)\ndiff --git a/Model/Application.hs b/Model/Application.hs\nnew file mode 100644\nindex 0000000..0977a69\n--- /dev/null\n+++ b/Model/Application.hs\n@@ -0,0 +1,11 @@\n+module Model.Application where\n+\n+import Import\n+\n+fetchApplicationVolunteerInterestsDB :: VolunteerApplicationId -> DB [Text]\n+fetchApplicationVolunteerInterestsDB application_id = fmap (map unValue) $\n+    select $\n+    from $ \\(vi `InnerJoin` i) -> do\n+    on_ (i ^. InterestId ==. vi ^. VolunteerInterestInterest)\n+    where_ (vi ^. VolunteerInterestVolunteer ==. val application_id)\n+    return (i ^. InterestDescription)\ndiff --git a/Model/Comment.hs b/Model/Comment.hs\nindex 1021411..d414b4e 100644\n--- a/Model/Comment.hs\n+++ b/Model/Comment.hs\n@@ -1,59 +1,69 @@\n module Model.Comment\n+    -- Types\n     ( ClosureMap\n+    , CommentMods(..)\n     , FlagMap\n+    , MaxDepth(..)\n+    , NoCommentReason(..)\n     , TicketMap\n-    , approveComment\n+    , addMaxDepth\n+    -- Utility functions\n     , buildCommentForest\n     , buildCommentTree\n-    , canDeleteComment\n-    , canEditComment\n-    , deleteComment\n-    , editComment\n-    , flagComment\n-    , getAllClosedRootComments\n-    , getAllOpenRootComments\n-    , getAllRootComments\n-    , getAncestorClosures\n-    , getAncestorClosures'\n-    , getCommentAncestors\n-    , getCommentDepth\n-    , getCommentDepth404\n-    , getCommentDescendants\n-    , getCommentDescendantsIds\n-    , getCommentDestination\n-    , getCommentFlagging\n-    , getCommentsDescendants\n-    , getCommentPage\n-    , getCommentPageId\n-    , getCommentPageEntity'\n-    , getCommentRethread\n-    , getCommentTags\n-    , getCommentsUsers\n-    , getTags\n-    , insertApprovedComment\n-    , insertUnapprovedComment\n-    , isApproved\n-    , isEvenDepth\n-    , isFlagged\n-    , isOddDepth\n-    , isTopLevel\n-    , makeClosureMap\n-    , makeFlagMap\n-    , makeModeratedComment\n-    , makeTicketMap\n+    , commentIsApproved\n+    , commentIsEvenDepth\n+    , commentIsFlagged\n+    , commentIsOddDepth\n+    , commentIsTopLevel\n+    , makeCommentUsersSet\n+    , makeApprovedComment\n     , newClosedCommentClosure\n     , newRetractedCommentClosure\n-    , rethreadComments\n-    , subGetCommentAncestors\n+    -- Database actions\n+    , approveCommentDB\n+    , deleteCommentDB\n+    , editCommentDB\n+    , flagCommentDB\n+    , fetchCommentAncestorClosuresDB\n+    , fetchCommentAncestorClosuresDB'\n+    , fetchCommentsAncestorClosuresDB\n+    , fetchCommentDB\n+    , fetchCommentAllDescendantsDB\n+    , fetchCommentAncestorsDB\n+    , fetchCommentCommentTagsDB\n+    , fetchCommentCommentTagsInDB\n+    , fetchCommentDepthDB\n+    , fetchCommentDepthFromMaybeParentIdDB\n+    , fetchCommentDepth404DB\n+    , fetchCommentDescendantsDB\n+    , fetchCommentDestinationDB\n+    , fetchCommentFlaggingDB\n+    , fetchCommentsDescendantsDB\n+    , fetchCommentWikiPageDB\n+    , fetchCommentRethreadDB\n+    , fetchCommentTagsDB\n+    , fetchCommentTagCommentTagsDB\n+    , filterCommentsDB\n+    , makeClosureMapDB\n+    , makeFlagMapDB\n+    , makeTicketMapDB\n+    , postApprovedCommentDB\n+    , postUnapprovedCommentDB\n+    , rethreadCommentsDB\n+    , subFetchCommentAncestorsDB\n+    , unsafeFetchCommentPageDB\n+    , unsafeFetchCommentPageIdDB\n     ) where\n \n import Import\n \n import           Model.Comment.Sql\n-import           Model.Message\n+import           Model.Notification\n+import           Model.Tag\n \n import qualified Control.Monad.State         as St\n import           Control.Monad.Writer.Strict (tell)\n+import           Data.Default                (Default, def)\n import           Data.Foldable               (Foldable)\n import qualified Data.Foldable               as F\n import qualified Data.Map                    as M\n@@ -61,93 +71,73 @@ import           Data.Maybe                  (fromJust)\n import qualified Data.Set                    as S\n import qualified Data.Text                   as T\n import           Data.Tree\n+import qualified Database.Persist            as P\n import           GHC.Exts                    (IsList(..))\n import           Yesod.Markdown              (Markdown(..))\n \n+--------------------------------------------------------------------------------\n+-- Types\n+\n type ClosureMap = Map CommentId CommentClosure\n type TicketMap  = Map CommentId (Entity Ticket)\n type FlagMap    = Map CommentId (Maybe Markdown, [FlagReason])\n \n-approveComment :: UserId -> CommentId -> Comment -> SDB ()\n-approveComment user_id comment_id comment = do\n-    lift upd\n-    tell [ECommentPosted comment_id comment]\n-  where\n-    upd = liftIO getCurrentTime >>= \\now ->\n-        update $ \\c -> do\n-        set c [ CommentModeratedTs =. val (Just now)\n-              , CommentModeratedBy =. val (Just user_id)\n-              ]\n-        where_ (c ^. CommentId ==. val comment_id)\n+-- | A root comment (with its own URL) might not be displayed. Why?\n+data NoCommentReason\n+    = CommentNotFound\n+    | CommentPermissionDenied\n \n-insertApprovedComment :: UTCTime\n-                      -> UTCTime\n-                      -> UserId\n-                      -> DiscussionId\n-                      -> Maybe CommentId\n-                      -> UserId\n-                      -> Markdown\n-                      -> Int\n-                      -> SDB CommentId\n-insertApprovedComment created_ts moderated_ts moderated_by discussion_id mparent_id user_id text depth =\n-    insertComment\n-      (Just moderated_ts)\n-      (Just moderated_by)\n-      ECommentPosted\n-      created_ts\n-      discussion_id\n-      mparent_id\n-      user_id\n-      text\n-      depth\n+-- | Data type used in makeCommentWidgetMod, containing modifications to comment-action-related\n+-- data structures.\n+data CommentMods = CommentMods\n+    { mod_comment          :: Comment          -> Comment\n+    , mod_earlier_closures :: [CommentClosure] -> [CommentClosure]\n+    , mod_user_map         :: Map UserId User  -> Map UserId User -- can't user UserMap here, circular dependency.\n+    , mod_closure_map      :: ClosureMap       -> ClosureMap\n+    , mod_ticket_map       :: TicketMap        -> TicketMap\n+    , mod_flag_map         :: FlagMap          -> FlagMap\n+    , mod_tag_map          :: TagMap           -> TagMap\n+    }\n \n-insertUnapprovedComment :: UTCTime\n-                        -> DiscussionId\n-                        -> Maybe CommentId\n-                        -> UserId\n-                        -> Markdown\n-                        -> Int\n-                        -> SDB CommentId\n-insertUnapprovedComment = insertComment Nothing Nothing ECommentPending\n-\n-insertComment :: Maybe UTCTime\n-              -> Maybe UserId\n-              -> (CommentId -> Comment -> SnowdriftEvent)\n-              -> UTCTime\n-              -> DiscussionId\n-              -> Maybe CommentId\n-              -> UserId\n-              -> Markdown\n-              -> Int\n-              -> SDB CommentId\n-insertComment mmoderated_ts mmoderated_by mk_event created_ts discussion_id mparent_id user_id text depth = do\n-    let comment = Comment\n-                    created_ts\n-                    mmoderated_ts\n-                    mmoderated_by\n-                    discussion_id\n-                    mparent_id\n-                    user_id\n-                    text\n-                    depth\n-    comment_id <- lift $ insert comment\n-    tell [mk_event comment_id comment]\n-    return comment_id\n+instance Default CommentMods where\n+    def = CommentMods id id id id id id id\n+\n+data MaxDepth\n+    = NoMaxDepth\n+    | MaxDepth Int\n+\n+instance Eq MaxDepth where\n+    NoMaxDepth == NoMaxDepth = True\n+    MaxDepth x == MaxDepth y = x == y\n+    _          == _          = False\n+\n+instance Ord MaxDepth where\n+    compare NoMaxDepth   (MaxDepth _) = GT\n+    compare NoMaxDepth   NoMaxDepth   = EQ\n+    compare (MaxDepth _) NoMaxDepth   = LT\n+    compare (MaxDepth x) (MaxDepth y) = compare x y\n \n-isApproved :: Comment -> Bool\n-isApproved = isJust . commentModeratedTs\n+addMaxDepth :: MaxDepth -> Int -> MaxDepth\n+addMaxDepth NoMaxDepth   _ = NoMaxDepth\n+addMaxDepth (MaxDepth x) y = MaxDepth (x + y)\n \n-isTopLevel :: Comment -> Bool\n-isTopLevel = (== 0) . commentDepth\n+--------------------------------------------------------------------------------\n+-- Utility functions\n \n-isEvenDepth :: Comment -> Bool\n-isEvenDepth comment = not (isTopLevel comment) && commentDepth comment `mod` 2 == 1\n+commentIsApproved :: Comment -> Bool\n+commentIsApproved = isJust . commentApprovedTs\n \n-isOddDepth :: Comment -> Bool\n-isOddDepth comment = not (isTopLevel comment) && not (isEvenDepth comment)\n+commentIsTopLevel :: Comment -> Bool\n+commentIsTopLevel = (== 0) . commentDepth\n \n-isFlagged :: CommentId -> DB Bool\n-isFlagged = fmap (maybe False (const True)) . getBy . UniqueCommentFlagging\n+commentIsEvenDepth :: Comment -> Bool\n+commentIsEvenDepth comment = not (commentIsTopLevel comment) && commentDepth comment `mod` 2 == 1\n+\n+commentIsOddDepth :: Comment -> Bool\n+commentIsOddDepth comment = not (commentIsTopLevel comment) && not (commentIsEvenDepth comment)\n+\n+commentIsFlagged :: CommentId -> DB Bool\n+commentIsFlagged = fmap (maybe False (const True)) . getBy . UniqueCommentFlagging\n \n -- | Build a tree of comments, given the root and replies. The replies are not necessarily\n -- direct or indirect descendants of the root, but rather may be siblings, nephews, etc.\n@@ -155,8 +145,8 @@ isFlagged = fmap (maybe False (const True)) . getBy . UniqueCommentFlagging\n --\n -- THIS FUNCTION RELIES ON THE SORT ORDER OF THE REPLIES! Specifically, they must be sorted\n -- in ascending-parent-id major, ascending-timestamp minor order.\n-buildCommentTree :: (Entity Comment, [Entity Comment]) -> Tree (Entity Comment)\n-buildCommentTree = unfoldTree step\n+buildCommentTree :: Entity Comment -> [Entity Comment] -> Tree (Entity Comment)\n+buildCommentTree r rs = unfoldTree step (r,rs)\n   where\n     step :: (Entity Comment, [Entity Comment]) -> (Entity Comment, [(Entity Comment, [Entity Comment])])\n     step (root, replies) = (root, children_and_their_descendants)\n@@ -174,63 +164,176 @@ buildCommentTree = unfoldTree step\n         isParentOf :: Entity Comment -> Entity Comment -> Bool\n         isParentOf (Entity parent_key _) (Entity _ child) = Just parent_key == commentParent child\n \n-buildCommentForest :: [Entity Comment]                                             -- root comments\n-                   -> [Entity Comment]                                             -- replies comments\n+buildCommentForest :: [Entity Comment] -- root comments\n+                   -> [Entity Comment] -- replies comments\n                    -> Forest (Entity Comment)\n-buildCommentForest roots replies = (map (buildCommentTree . (, replies))) roots\n+buildCommentForest roots replies = (map (flip buildCommentTree replies)) roots\n \n-canDeleteComment :: UserId -> Entity Comment -> DB Bool\n-canDeleteComment user_id (Entity comment_id comment) = do\n-    if commentUser comment /= user_id\n-        then return False\n-        else do\n-          descendants_ids <- getCommentDescendantsIds comment_id\n-          if null descendants_ids\n-              then return True\n-              else return False\n+newClosedCommentClosure, newRetractedCommentClosure :: MonadIO m => UserId -> Markdown -> CommentId -> m CommentClosure\n+newClosedCommentClosure    = newCommentClosure Closed\n+newRetractedCommentClosure = newCommentClosure Retracted\n \n-canEditComment :: UserId -> Comment -> Bool\n-canEditComment user_id = (user_id ==) . commentUser\n+newCommentClosure :: MonadIO m => ClosureType -> UserId -> Markdown -> CommentId -> m CommentClosure\n+newCommentClosure closure_type user_id reason comment_id =\n+    (\\now -> CommentClosure now user_id closure_type reason comment_id) `liftM` liftIO getCurrentTime\n+\n+-- | Construct a comment, auto-approved by 'this' User (because they are established).\n+makeApprovedComment :: MonadIO m => UserId -> DiscussionId -> Maybe CommentId -> Markdown -> Int -> m Comment\n+makeApprovedComment user_id discussion_id parent_comment comment_text depth = do\n+    now <- liftIO getCurrentTime\n+    return $ Comment\n+                 now\n+                 (Just now)\n+                 (Just user_id)\n+                 discussion_id\n+                 parent_comment\n+                 user_id\n+                 comment_text\n+                 depth\n+\n+-- | Get the set of Users that have posted the given Foldable of comments.\n+makeCommentUsersSet :: Foldable f => f (Entity Comment) -> Set UserId\n+makeCommentUsersSet = F.foldMap (S.singleton . commentUser . entityVal)\n+\n+--------------------------------------------------------------------------------\n+-- Database actions\n+\n+approveCommentDB :: UserId -> CommentId -> Comment -> SDB ()\n+approveCommentDB user_id comment_id comment = do\n+    now <- liftIO getCurrentTime\n+    -- Do an in-memory adjustment of the comment with exactly the same changes\n+    -- as 'upd' below (so we can avoid hitting the database).\n+    let updated_comment = comment\n+          { commentApprovedTs = Just now\n+          , commentApprovedBy = Just user_id\n+          }\n+    lift $ do\n+        updateComment now\n+        deleteUnapprovedCommentNotifications\n+    tell [ECommentPosted comment_id updated_comment]\n+  where\n+    updateComment now =\n+        update $ \\c -> do\n+        set c [ CommentApprovedTs =. val (Just now)\n+              , CommentApprovedBy =. val (Just user_id)\n+              ]\n+        where_ (c ^. CommentId ==. val comment_id)\n+\n+    -- Delete all notifications sent about this pending comment, as they no longer apply.\n+    -- Also deletes the UnapprovedCommentNotification entities, the EventNotificationSent,\n+    -- and any other rows with a foreign key on NotificationId.\n+    deleteUnapprovedCommentNotifications = do\n+        notif_ids <- fmap (map unValue) $\n+                         select $\n+                         from $ \\unc -> do\n+                         where_ (unc ^. UnapprovedCommentNotificationComment ==. val comment_id)\n+                         return (unc ^. UnapprovedCommentNotificationNotification)\n+        deleteCascadeWhere [NotificationId P.<-. notif_ids]\n+\n+insertApprovedCommentDB\n+        :: UTCTime\n+        -> DiscussionId\n+        -> Maybe CommentId\n+        -> UserId\n+        -> Markdown\n+        -> Int\n+        -> SDB CommentId\n+insertApprovedCommentDB created_ts discussion_id mparent_id user_id text depth =\n+    insertCommentDB\n+      (Just created_ts)\n+      (Just user_id)\n+      ECommentPosted\n+      created_ts\n+      discussion_id\n+      mparent_id\n+      user_id\n+      text\n+      depth\n+\n+insertUnapprovedCommentDB\n+        :: UTCTime\n+        -> DiscussionId\n+        -> Maybe CommentId\n+        -> UserId\n+        -> Markdown\n+        -> Int\n+        -> SDB CommentId\n+insertUnapprovedCommentDB = insertCommentDB Nothing Nothing ECommentPending\n+\n+insertCommentDB :: Maybe UTCTime\n+                -> Maybe UserId\n+                -> (CommentId -> Comment -> SnowdriftEvent)\n+                -> UTCTime\n+                -> DiscussionId\n+                -> Maybe CommentId\n+                -> UserId\n+                -> Markdown\n+                -> Int\n+                -> SDB CommentId\n+insertCommentDB mapproved_ts mapproved_by mk_event created_ts discussion_id mparent_id user_id text depth = do\n+    let comment = Comment\n+                    created_ts\n+                    mapproved_ts\n+                    mapproved_by\n+                    discussion_id\n+                    mparent_id\n+                    user_id\n+                    text\n+                    depth\n+    comment_id <- lift $ insert comment\n+    tell [mk_event comment_id comment]\n+    return comment_id\n+\n+-- | Fetch a comment from the DB, subject to viewing permissions.\n+fetchCommentDB :: CommentId -> ExprCommentCond -> DB (Either NoCommentReason Comment)\n+fetchCommentDB comment_id has_permission = get comment_id >>= \\case\n+    Nothing -> return (Left CommentNotFound)\n+    -- Hooray, the comment exists, now toss it and re-query the database with the\n+    -- provided permission conditions. How else would we be able to differentiate\n+    -- a non-existent comment and a comment the user doesn't have permission to\n+    -- view?\n+    Just _ -> fmap (maybe (Left CommentPermissionDenied) (Right . entityVal) . listToMaybe) $\n+                  select $\n+                  from $ \\c -> do\n+                  where_ $\n+                      c ^. CommentId ==. val comment_id &&.\n+                      has_permission c\n+                  return c\n \n -- | Delete-cascade a comment from the database.\n-deleteComment :: CommentId -> DB ()\n-deleteComment = deleteCascade\n+deleteCommentDB :: CommentId -> DB ()\n+deleteCommentDB = deleteCascade\n \n -- | Edit a comment's text. If the comment was flagged, unflag it and send a\n--- message to the flagger.\n-editComment :: CommentId -> Markdown -> SYDB ()\n-editComment comment_id text = do\n+-- notification to the flagger.\n+editCommentDB :: CommentId -> Markdown -> SYDB ()\n+editCommentDB comment_id text = do\n     lift updateCommentText\n-    lift (getCommentFlagging comment_id) >>= \\case\n+    lift (fetchCommentFlaggingDB comment_id) >>= \\case\n         Nothing -> return ()\n         Just (Entity comment_flagging_id CommentFlagging{..}) -> do\n-            let permalink_route = DiscussCommentR\n-                                    commentFlaggingProjectHandle\n-                                    commentFlaggingTarget\n-                                    comment_id\n-            permalink_text <- lift $ getUrlRender <*> pure permalink_route\n-            let message_text = Markdown $ "A comment you flagged has been edited and reposted to the site. You can view it [here](" <> permalink_text <> ")."\n-            lift $ deleteCascade comment_flagging_id -- delete flagging and all flagging reasons with it.\n-            snowdrift_id <- lift getSnowdriftId\n-            insertMessage_ MessageDirect (Just snowdrift_id) Nothing (Just $ commentFlaggingFlagger) message_text True\n+            permalink_text <- lift (getUrlRender <*> pure (CommentDirectLinkR comment_id))\n+            let notif_text = Markdown $ "A comment you flagged has been edited and reposted to the site. You can view it [here](" <> permalink_text <> ")."\n+            lift (deleteCascade comment_flagging_id) -- delete flagging and all flagging reasons with it.\n+            sendNotificationDB_ NotifFlagRepost commentFlaggingFlagger Nothing notif_text\n   where\n     updateCommentText =\n         update $ \\c -> do\n         set c [ CommentText =. val text ]\n         where_ (c ^. CommentId ==. val comment_id)\n \n--- | Flag a comment. Send a message to the poster about the flagging. Return whether\n+-- | Flag a comment. Send a notification to the poster about the flagging. Return whether\n -- or not the flag was successful (fails if the comment was already flagged.)\n-flagComment :: Text -> Text -> CommentId -> Text -> UserId -> [FlagReason] -> Maybe Markdown -> SYDB Bool\n-flagComment project_handle target comment_id permalink_route flagger_id reasons message = do\n-    poster_id <- lift $ commentUser <$> get404 comment_id\n+flagCommentDB :: CommentId -> Text -> UserId -> [FlagReason] -> Maybe Markdown -> SYDB Bool\n+flagCommentDB comment_id permalink_route flagger_id reasons message = do\n+    poster_id <- lift (commentUser <$> get404 comment_id)\n     now <- liftIO getCurrentTime\n-    lift (insertUnique (CommentFlagging now flagger_id comment_id project_handle target message)) >>= \\case\n+    lift (insertUnique (CommentFlagging now flagger_id comment_id message)) >>= \\case\n         Nothing -> return False\n         Just flagging_id -> do\n             lift $ void $ insertMany (map (CommentFlaggingReason flagging_id) reasons)\n \n-            let message_text = Markdown . T.unlines $\n+            let notif_text = Markdown . T.unlines $\n                     [ "Another user flagged your comment as not meeting the standards of the Code of Conduct. We *want* your involvement as long as it remains respectful and friendly, so please don’t feel discouraged."\n                     , ""\n                     , "Please follow the link below for clarification and suggestions the flagger may have offered, and take this chance to improve your tone and clarify any misunderstanding. Your newly edited comment will then be publicly visible again."\n@@ -239,13 +342,63 @@ flagComment project_handle target comment_id permalink_route flagger_id reasons\n                     , ""\n                     , "[link to flagged comment](" <> permalink_route <> ")"\n                     ]\n-            snowdrift_id <- lift getSnowdriftId\n-            insertMessage_ MessageDirect (Just snowdrift_id) Nothing (Just poster_id) message_text True\n+            sendNotificationDB_ NotifFlag poster_id Nothing notif_text\n             return True\n \n+-- | Post an new (approved) Comment.\n+postApprovedCommentDB :: UserId -> Maybe CommentId -> DiscussionId -> Markdown -> SDB CommentId\n+postApprovedCommentDB = postComment insertApprovedCommentDB\n+\n+postUnapprovedCommentDB :: UserId -> Maybe CommentId -> DiscussionId -> Markdown -> SDB CommentId\n+postUnapprovedCommentDB = postComment insertUnapprovedCommentDB\n+\n+postComment\n+        :: (UTCTime -> DiscussionId -> Maybe CommentId -> UserId -> Markdown -> Int -> SDB CommentId)\n+        -> UserId\n+        -> Maybe CommentId\n+        -> DiscussionId\n+        -> Markdown\n+        -> SDB CommentId\n+postComment insert_comment user_id mparent_id discussion_id contents = do\n+    (now, depth) <- lift $ (,)\n+        <$> liftIO getCurrentTime\n+        <*> fetchCommentDepthFromMaybeParentIdDB mparent_id\n+\n+    comment_id <- insert_comment now discussion_id mparent_id user_id contents depth\n+\n+    let content = T.lines (unMarkdown contents)\n+        tickets = map T.strip $ mapMaybe (T.stripPrefix "ticket:") content\n+        tags    = map T.strip $ mconcat $ map (T.splitOn ",") $ mapMaybe (T.stripPrefix "tags:") content\n+\n+    lift $ do\n+        forM_ tickets $ \\ticket -> insert_ (Ticket now now ticket comment_id)\n+        forM_ tags $ \\tag -> do\n+            tag_id <- either entityKey id <$> insertBy (Tag tag)\n+            insert_ (CommentTag comment_id tag_id user_id 1)\n+\n+        case mparent_id of\n+            Nothing -> return ()\n+            Just parent_id -> mapM_ (insert_ . CommentAncestor comment_id) =<< (parent_id:) <$> fetchCommentAncestorsDB parent_id\n+\n+        update $ \\t -> do\n+         set t [TicketUpdatedTs =. val now]\n+         where_ (t ^. TicketComment `in_` subFetchCommentAncestorsDB comment_id)\n+\n+    return comment_id\n+\n+-- | Filter a list of comments per the provided permission filter.\n+filterCommentsDB :: [CommentId] -> ExprCommentCond -> DB [CommentId]\n+filterCommentsDB comment_ids has_permission = fmap (map unValue) $\n+    select $\n+    from $ \\c -> do\n+    where_ $\n+        c ^. CommentId `in_` valList comment_ids &&.\n+        has_permission c\n+    return (c ^. CommentId)\n+\n -- | Get all ancestors that have been closed.\n-getAncestorClosures :: CommentId -> DB [CommentClosure]\n-getAncestorClosures comment_id = fmap (map entityVal) $\n+fetchCommentAncestorClosuresDB :: CommentId -> DB [CommentClosure]\n+fetchCommentAncestorClosuresDB comment_id = fmap (map entityVal) $\n     select $\n     from $ \\(ca `InnerJoin` cc) -> do\n     on_ (ca ^. CommentAncestorAncestor ==. cc ^. CommentClosureComment)\n@@ -254,48 +407,62 @@ getAncestorClosures comment_id = fmap (map entityVal) $\n     return cc\n \n -- | Get all ancestors, including this comment, that have been closed.\n-getAncestorClosures' :: CommentId -> DB [CommentClosure]\n-getAncestorClosures' comment_id = do\n-    all_comment_ids <- (comment_id :) <$> getCommentAncestors comment_id\n+fetchCommentAncestorClosuresDB' :: CommentId -> DB [CommentClosure]\n+fetchCommentAncestorClosuresDB' comment_id = do\n+    all_comment_ids <- (comment_id :) <$> fetchCommentAncestorsDB comment_id\n     fmap (map entityVal) $\n         select $\n         from $ \\cc -> do\n         where_ (cc ^. CommentClosureComment `in_` valList all_comment_ids)\n         return cc\n \n+-- | Get all CommentClosures of any of the given Comments' ancestors, grouped by\n+-- the given Comments.\n+fetchCommentsAncestorClosuresDB :: [CommentId] -> DB (Map CommentId [CommentClosure])\n+fetchCommentsAncestorClosuresDB comment_ids = fmap (foldr step mempty) $\n+    select $\n+    from $ \\(ca `InnerJoin` cc) -> do\n+    on_ (ca ^. CommentAncestorAncestor ==. cc ^. CommentClosureComment)\n+    orderBy [asc (cc ^. CommentClosureComment)]\n+    where_ (ca ^. CommentAncestorComment `in_` valList comment_ids)\n+    return (ca ^. CommentAncestorComment, cc)\n+  where\n+    step :: (Value CommentId, Entity CommentClosure) -> Map CommentId [CommentClosure] -> Map CommentId [CommentClosure]\n+    step (Value c, Entity _ cc) = M.insertWith (++) c [cc]\n+\n -- | Get a comment's ancestors' ids.\n-getCommentAncestors :: CommentId -> DB [CommentId]\n-getCommentAncestors = fmap (map unValue) . select . querAncestors\n+fetchCommentAncestorsDB :: CommentId -> DB [CommentId]\n+fetchCommentAncestorsDB = fmap (map unValue) . select . querCommentAncestors\n+\n+subFetchCommentAncestorsDB :: CommentId -> SqlExpr (ValueList CommentId)\n+subFetchCommentAncestorsDB = subList_select . querCommentAncestors\n \n-subGetCommentAncestors :: CommentId -> SqlExpr (ValueList CommentId)\n-subGetCommentAncestors = subList_select . querAncestors\n+fetchCommentDepthDB :: CommentId -> DB Int\n+fetchCommentDepthDB = fmap commentDepth . getJust\n \n-getCommentDepth :: CommentId -> DB Int\n-getCommentDepth = fmap commentDepth . getJust\n+-- | Get the depth of a comment, given (maybe) its parent's CommentId.\n+fetchCommentDepthFromMaybeParentIdDB :: Maybe CommentId -> DB Int\n+fetchCommentDepthFromMaybeParentIdDB = maybe (return 0) (fmap (+1) . fetchCommentDepthDB)\n \n-getCommentDepth404 :: CommentId -> Handler Int\n-getCommentDepth404 = fmap commentDepth . runYDB . get404\n+fetchCommentDepth404DB :: CommentId -> Handler Int\n+fetchCommentDepth404DB = fmap commentDepth . runYDB . get404\n \n -- | Get the CommentFlagging even for this Comment, if there is one.\n-getCommentFlagging :: CommentId -> DB (Maybe (Entity CommentFlagging))\n-getCommentFlagging = getBy . UniqueCommentFlagging\n+fetchCommentFlaggingDB :: CommentId -> DB (Maybe (Entity CommentFlagging))\n+fetchCommentFlaggingDB = getBy . UniqueCommentFlagging\n \n--- | Partial function.\n-getCommentPage :: CommentId -> DB WikiPage\n-getCommentPage = fmap entityVal . getCommentPageEntity\n+unsafeFetchCommentPageDB :: CommentId -> DB WikiPage\n+unsafeFetchCommentPageDB = fmap entityVal . unsafeFetchCommentPageEntityDB\n \n--- | Partial function.\n-getCommentPageId :: CommentId -> DB WikiPageId\n-getCommentPageId = fmap entityKey . getCommentPageEntity\n+unsafeFetchCommentPageIdDB :: CommentId -> DB WikiPageId\n+unsafeFetchCommentPageIdDB = fmap entityKey . unsafeFetchCommentPageEntityDB\n \n--- | Partial function. Fails if the given Comment is not on a WikiPage, but some\n--- other Discussion.\n-getCommentPageEntity :: CommentId -> DB (Entity WikiPage)\n-getCommentPageEntity = fmap fromJust . getCommentPageEntity'\n+-- | Fails if the given Comment is not on a WikiPage, but some other Discussion.\n+unsafeFetchCommentPageEntityDB :: CommentId -> DB (Entity WikiPage)\n+unsafeFetchCommentPageEntityDB = fmap fromJust . fetchCommentWikiPageDB\n \n--- | Safe version. TODO: Rename above 'unsafeGetCommentPageEntity'\n-getCommentPageEntity' :: CommentId -> DB (Maybe (Entity WikiPage))\n-getCommentPageEntity' comment_id = fmap listToMaybe $\n+fetchCommentWikiPageDB :: CommentId -> DB (Maybe (Entity WikiPage))\n+fetchCommentWikiPageDB comment_id = fmap listToMaybe $\n     select $\n     from $ \\(c `InnerJoin` p) -> do\n     on_ (c ^. CommentDiscussion ==. p ^. WikiPageDiscussion)\n@@ -303,100 +470,84 @@ getCommentPageEntity' comment_id = fmap listToMaybe $\n     return p\n \n -- | Get the CommentId this CommentId was rethreaded to, if it was.\n-getCommentRethread :: CommentId -> DB (Maybe CommentId)\n-getCommentRethread comment_id = fmap unValue . listToMaybe <$> (\n+fetchCommentRethreadDB :: CommentId -> DB (Maybe CommentId)\n+fetchCommentRethreadDB comment_id = fmap unValue . listToMaybe <$> (\n     select $\n     from $ \\cr -> do\n     where_ $ cr ^. CommentRethreadOldComment ==. val comment_id\n     return $ cr ^. CommentRethreadNewComment)\n \n -- | Get a Comment's CommentTags.\n-getCommentTags :: CommentId -> DB [Entity CommentTag]\n-getCommentTags comment_id =\n+fetchCommentCommentTagsDB :: CommentId -> DB [CommentTag]\n+fetchCommentCommentTagsDB comment_id = fmap (map entityVal) $\n+    select $\n+    from $ \\ct -> do\n+    where_ (ct ^. CommentTagComment ==. val comment_id)\n+    return ct\n+\n+fetchCommentCommentTagsInDB :: [CommentId] -> DB [CommentTag]\n+fetchCommentCommentTagsInDB comment_ids = fmap (map entityVal) $\n     select $\n-    from $ \\comment_tag -> do\n-    where_ $ comment_tag ^. CommentTagComment ==. val comment_id\n-    return comment_tag\n+    from $ \\ct -> do\n+    where_ (ct ^. CommentTagComment `in_` valList comment_ids)\n+    return ct\n \n--- | Get a Comment's descendants' ids (don't filter hidden or unmoderated comments).\n-getCommentDescendantsIds :: CommentId -> DB [CommentId]\n-getCommentDescendantsIds = fmap (map unValue) . select . querDescendants\n+-- | Get a Comment's descendants' ids (don't filter hidden or unapproved comments).\n+fetchCommentAllDescendantsDB :: CommentId -> DB [CommentId]\n+fetchCommentAllDescendantsDB = fmap (map unValue) . select . querCommentDescendants\n \n -- | Get all descendants of the given root comment.\n-getCommentDescendants :: Maybe UserId -> ProjectId -> CommentId -> DB [Entity Comment]\n-getCommentDescendants mviewer_id project_id root_id =\n+fetchCommentDescendantsDB :: CommentId -> ExprCommentCond -> DB [Entity Comment]\n+fetchCommentDescendantsDB comment_id has_permission =\n     select $\n     from $ \\c -> do\n     where_ $\n-        c ^. CommentId `in_` subList_select (querDescendants root_id) &&.\n-        exprPermissionFilter mviewer_id (val project_id) c\n+        c ^. CommentId `in_` subList_select (querCommentDescendants comment_id) &&.\n+        has_permission c\n     -- DO NOT change ordering here! buildCommentTree relies on it.\n     orderBy [asc (c ^. CommentParent), asc (c ^. CommentCreatedTs)]\n     return c\n \n -- | Get all descendants of all given root comments.\n-getCommentsDescendants :: Maybe UserId -> ProjectId -> [CommentId] -> DB [Entity Comment]\n-getCommentsDescendants mviewer_id project_id root_ids =\n+fetchCommentsDescendantsDB :: [CommentId] -> ExprCommentCond -> DB [Entity Comment]\n+fetchCommentsDescendantsDB comment_ids has_permission =\n     select $\n     from $ \\c -> do\n     where_ $\n-        c ^. CommentId `in_` subList_select (querAllDescendants root_ids) &&.\n-        exprPermissionFilter mviewer_id (val project_id) c\n+        c ^. CommentId `in_` subList_select (querCommentsDescendants comment_ids) &&.\n+        has_permission c\n     -- DO NOT change ordering here! buildCommentTree relies on it.\n     orderBy [asc (c ^. CommentParent), asc (c ^. CommentCreatedTs)]\n     return c\n \n -- | Get the "true" target of this CommentId (which may be itself, if not rethreaded -\n -- otherwise, ride the rethread train to the end)\n-getCommentDestination :: CommentId -> YDB CommentId\n-getCommentDestination comment_id = do\n+fetchCommentDestinationDB :: CommentId -> YDB CommentId\n+fetchCommentDestinationDB comment_id = do\n     void $ get404 comment_id -- make sure the comment even exists, so this function terminates.\n-    getCommentRethread comment_id >>= maybe (return comment_id) getCommentDestination\n-\n--- | Get all Comments on a Discussion that are root comments.\n-getAllRootComments :: Maybe UserId -> ProjectId -> DiscussionId -> DB [Entity Comment]\n-getAllRootComments mviewer_id project_id discussion_id =\n-    select $\n-    from $ \\c -> do\n-    where_ $\n-        exprOnDiscussion discussion_id c &&.\n-        exprRoot c &&.\n-        exprPermissionFilter mviewer_id (val project_id) c\n-    return c\n-\n-getAllClosedRootComments :: Maybe UserId -> ProjectId -> DiscussionId -> DB [Entity Comment]\n-getAllClosedRootComments mviewer_id project_id discussion_id =\n-    select $\n-    from $ \\c -> do\n-    where_ $\n-        exprOnDiscussion discussion_id c &&.\n-        exprRoot c &&.\n-        exprClosed c &&.\n-        exprPermissionFilter mviewer_id (val project_id) c\n-    return c\n-\n-getAllOpenRootComments :: Maybe UserId -> ProjectId -> DiscussionId -> DB [Entity Comment]\n-getAllOpenRootComments mviewer_id project_id discussion_id =\n-    select $\n-    from $ \\c -> do\n-    where_ $\n-        exprOnDiscussion discussion_id c &&.\n-        exprRoot c &&.\n-        exprOpen c &&.\n-        exprPermissionFilter mviewer_id (val project_id) c\n-    return c\n+    fetchCommentRethreadDB comment_id >>= maybe (return comment_id) fetchCommentDestinationDB\n \n -- | Get a Comment's Tags.\n-getTags :: CommentId -> DB [Entity Tag]\n-getTags comment_id =\n+fetchCommentTagsDB :: CommentId -> DB [Entity Tag]\n+fetchCommentTagsDB comment_id =\n     select $\n     from $ \\(ct `InnerJoin` t) -> do\n     on_ (ct ^. CommentTagTag ==. t ^. TagId)\n     where_ (ct ^. CommentTagComment ==. val comment_id)\n     return t\n \n-makeClosureMap :: (IsList c, CommentId ~ Item c) => c -> DB ClosureMap\n-makeClosureMap comment_ids = fmap (M.fromList . map ((commentClosureComment &&& id) . entityVal)) $\n+-- | Get a Comment's CommentTags for a specific Tag.\n+fetchCommentTagCommentTagsDB :: CommentId -> TagId -> DB [CommentTag]\n+fetchCommentTagCommentTagsDB comment_id tag_id = fmap (map entityVal) $\n+    select $\n+    from $ \\ct -> do\n+    where_ $\n+        ct ^. CommentTagComment ==. val comment_id &&.\n+        ct ^. CommentTagTag ==. val tag_id\n+    return ct\n+\n+makeClosureMapDB :: (IsList c, CommentId ~ Item c) => c -> DB ClosureMap\n+makeClosureMapDB comment_ids = fmap (M.fromList . map ((commentClosureComment &&& id) . entityVal)) $\n     select $\n     from $ \\c -> do\n     where_ (c ^. CommentClosureComment `in_` valList comment_ids)\n@@ -404,15 +555,15 @@ makeClosureMap comment_ids = fmap (M.fromList . map ((commentClosureComment &&&\n \n -- Given a collection of CommentId, make a map from CommentId to Entity Ticket. Comments that\n -- are not tickets will simply not be in the map.\n-makeTicketMap :: (IsList c, CommentId ~ Item c) => c -> DB TicketMap\n-makeTicketMap comment_ids = fmap (M.fromList . map ((ticketComment . entityVal) &&& id)) $\n+makeTicketMapDB :: (IsList c, CommentId ~ Item c) => c -> DB TicketMap\n+makeTicketMapDB comment_ids = fmap (M.fromList . map ((ticketComment . entityVal) &&& id)) $\n     select $\n     from $ \\t -> do\n     where_ (t ^. TicketComment `in_` valList comment_ids)\n     return t\n \n-makeFlagMap :: (IsList c, CommentId ~ Item c) => c -> DB FlagMap\n-makeFlagMap comment_ids = mkFlagMap <$> getCommentFlaggings\n+makeFlagMapDB :: (IsList c, CommentId ~ Item c) => c -> DB FlagMap\n+makeFlagMapDB comment_ids = mkFlagMap <$> getCommentFlaggings\n   where\n     getCommentFlaggings :: DB [(CommentId, Maybe Markdown, FlagReason)]\n     getCommentFlaggings = fmap (map unwrapValues) $\n@@ -428,34 +579,8 @@ makeFlagMap comment_ids = mkFlagMap <$> getCommentFlaggings\n         combine :: (Maybe Markdown, [FlagReason]) -> (Maybe Markdown, [FlagReason]) -> (Maybe Markdown, [FlagReason])\n         combine (message, reasons1) (_, reasons2) = (message, reasons1 <> reasons2)\n \n-newClosedCommentClosure, newRetractedCommentClosure :: MonadIO m => UserId -> Markdown -> CommentId -> m CommentClosure\n-newClosedCommentClosure    = newCommentClosure Closed\n-newRetractedCommentClosure = newCommentClosure Retracted\n-\n-newCommentClosure :: MonadIO m => ClosureType -> UserId -> Markdown -> CommentId -> m CommentClosure\n-newCommentClosure closure_type user_id reason comment_id =\n-    (\\now -> CommentClosure now user_id closure_type reason comment_id) `liftM` liftIO getCurrentTime\n-\n--- | Construct a comment, auto-moderated by 'this' User (because they are established).\n-makeModeratedComment :: MonadIO m => UserId -> DiscussionId -> Maybe CommentId -> Markdown -> Int -> m Comment\n-makeModeratedComment user_id discussion_id parent_comment comment_text depth = do\n-    now <- liftIO getCurrentTime\n-    return $ Comment\n-                 now\n-                 (Just now)\n-                 (Just user_id)\n-                 discussion_id\n-                 parent_comment\n-                 user_id\n-                 comment_text\n-                 depth\n-\n--- | Get the set of Users that have posted the given Foldable of comments.\n-getCommentsUsers :: Foldable f => f (Entity Comment) -> Set UserId\n-getCommentsUsers = F.foldMap (S.singleton . commentUser . entityVal)\n-\n-rethreadComments :: RethreadId -> Int -> Maybe CommentId -> DiscussionId -> [CommentId] -> DB [CommentId]\n-rethreadComments rethread_id depth_offset maybe_new_parent_id new_discussion_id comment_ids = do\n+rethreadCommentsDB :: RethreadId -> Int -> Maybe CommentId -> DiscussionId -> [CommentId] -> DB [CommentId]\n+rethreadCommentsDB rethread_id depth_offset maybe_new_parent_id new_discussion_id comment_ids = do\n     new_comment_ids <- flip St.evalStateT M.empty $ forM comment_ids $ \\ comment_id -> do\n         rethreads <- St.get\n \ndiff --git a/Model/Comment/ActionPermissions.hs b/Model/Comment/ActionPermissions.hs\nnew file mode 100644\nindex 0000000..32f7e4d\n--- /dev/null\n+++ b/Model/Comment/ActionPermissions.hs\n@@ -0,0 +1,62 @@\n+module Model.Comment.ActionPermissions\n+    ( CommentActionPermissions(..)\n+    , MakeCommentActionPermissions\n+    , makeProjectCommentActionPermissions\n+    ) where\n+\n+import Import\n+\n+import Model.Comment\n+import Model.User\n+\n+type MakeCommentActionPermissions = Entity Comment -> Handler CommentActionPermissions\n+\n+data CommentActionPermissions = CommentActionPermissions\n+    { can_add_tag   :: Bool\n+    , can_approve   :: Bool\n+    , can_close     :: Bool\n+    , can_delete    :: Bool\n+    , can_edit      :: Bool\n+    , can_establish :: Bool\n+    , can_flag      :: Bool\n+    , can_reply     :: Bool\n+    , can_rethread  :: Bool\n+    , can_retract   :: Bool\n+    }\n+\n+-- | Action permissions that apply to both a Project discussion and a Projects WikiPage discussion.\n+makeProjectCommentActionPermissions :: Text -> MakeCommentActionPermissions\n+makeProjectCommentActionPermissions project_handle comment_entity@(Entity comment_id comment) = do\n+    maybeAuth >>= \\case\n+        Nothing -> return $ CommentActionPermissions\n+            { can_add_tag   = False\n+            , can_approve   = False\n+            , can_close     = False\n+            , can_delete    = False\n+            , can_edit      = False\n+            , can_establish = False\n+            , can_flag      = False\n+            , can_reply     = False\n+            , can_rethread  = False\n+            , can_retract   = False\n+            }\n+        Just (Entity viewer_id viewer) -> do\n+            let poster_id = commentUser comment\n+            (poster, is_mod, can_del, is_flagged) <- runYDB $ do\n+                Entity project_id _ <- getBy404 (UniqueProjectHandle project_handle)\n+                (,,,) <$> get404 poster_id\n+                      <*> userIsProjectModeratorDB viewer_id project_id\n+                      <*> userCanDeleteCommentDB viewer_id comment_entity\n+                      <*> commentIsFlagged comment_id\n+            return $ CommentActionPermissions\n+                { can_add_tag   = userIsEstablished viewer\n+                , can_approve   = is_mod && (not . commentIsApproved) comment\n+                , can_close     = userCanCloseComment viewer\n+                , can_delete    = can_del\n+                , can_edit      = userCanEditComment viewer_id comment\n+                , can_establish = is_mod && userIsUnestablished poster\n+                , can_flag      = userIsEstablished viewer && viewer_id /= poster_id && not is_flagged\n+                , can_reply     = True\n+                , can_rethread  = poster_id == viewer_id || is_mod\n+                , can_retract   = poster_id == viewer_id\n+                }\ndiff --git a/Model/Comment/HandlerInfo.hs b/Model/Comment/HandlerInfo.hs\nnew file mode 100644\nindex 0000000..ab42c17\n--- /dev/null\n+++ b/Model/Comment/HandlerInfo.hs\n@@ -0,0 +1,31 @@\n+module Model.Comment.HandlerInfo where\n+\n+import Import\n+\n+import Model.Comment.ActionPermissions\n+import Model.Comment.Routes\n+import Model.Comment.Sql\n+\n+-- | Data type that packages together the necessary data for\n+-- handling a Comment page. The viewing permission, routes,\n+-- and action permissions vary depending on exactly where\n+-- the comment is.\n+data CommentHandlerInfo = CommentHandlerInfo\n+    { commentHandlerHasPermission         :: ExprCommentCond\n+    , commentHandlerRoutes                :: CommentRoutes\n+    , commentHandlerMakeActionPermissions :: MakeCommentActionPermissions\n+    }\n+\n+projectCommentHandlerInfo :: Maybe UserId -> ProjectId -> Text -> CommentHandlerInfo\n+projectCommentHandlerInfo muser_id project_id project_handle = CommentHandlerInfo\n+    { commentHandlerHasPermission         = exprCommentProjectPermissionFilter muser_id (val project_id)\n+    , commentHandlerRoutes                = projectCommentRoutes project_handle\n+    , commentHandlerMakeActionPermissions = makeProjectCommentActionPermissions project_handle\n+    }\n+\n+wikiPageCommentHandlerInfo :: Maybe UserId -> ProjectId -> Text -> Text -> CommentHandlerInfo\n+wikiPageCommentHandlerInfo muser_id project_id project_handle target = CommentHandlerInfo\n+    { commentHandlerHasPermission         = exprCommentProjectPermissionFilter muser_id (val project_id)\n+    , commentHandlerRoutes                = wikiPageCommentRoutes project_handle target\n+    , commentHandlerMakeActionPermissions = makeProjectCommentActionPermissions project_handle\n+    }\ndiff --git a/Model/Comment/Internal.hs b/Model/Comment/Internal.hs\nindex a07b1c9..2edd9c6 100644\n--- a/Model/Comment/Internal.hs\n+++ b/Model/Comment/Internal.hs\n@@ -3,7 +3,7 @@ module Model.Comment.Internal where\n import Prelude\n \n import Database.Persist.TH\n-import Data.Text (Text)\n+import Data.Text           (Text)\n \n data ClosureType\n     = Retracted\ndiff --git a/Model/Comment/Routes.hs b/Model/Comment/Routes.hs\nnew file mode 100644\nindex 0000000..6746b03\n--- /dev/null\n+++ b/Model/Comment/Routes.hs\n@@ -0,0 +1,64 @@\n+module Model.Comment.Routes where\n+\n+import Import\n+\n+-- | Collection of Routes that can be made from a CommentId (because Comments on\n+-- various places around the site have different URLs, even though their\n+-- IDs are unique).\n+data CommentRoutes = CommentRoutes\n+    { comment_route_add_tag   :: CommentId -> Route App\n+    , comment_route_approve   :: CommentId -> Route App\n+    , comment_route_close     :: CommentId -> Route App\n+    , comment_route_delete    :: CommentId -> Route App\n+    , comment_route_edit      :: CommentId -> Route App\n+    , comment_route_flag      :: CommentId -> Route App\n+    , comment_route_permalink :: CommentId -> Route App\n+    , comment_route_reply     :: CommentId -> Route App\n+    , comment_route_rethread  :: CommentId -> Route App\n+    , comment_route_retract   :: CommentId -> Route App\n+    , comment_route_tag       :: CommentId -> TagId -> Route App\n+    }\n+\n+dummyCommentRoutes :: CommentRoutes\n+dummyCommentRoutes = CommentRoutes\n+    (const HomeR)\n+    (const HomeR)\n+    (const HomeR)\n+    (const HomeR)\n+    (const HomeR)\n+    (const HomeR)\n+    (const HomeR)\n+    (const HomeR)\n+    (const HomeR)\n+    (const HomeR)\n+    (\\_ _ -> HomeR)\n+\n+projectCommentRoutes :: Text -> CommentRoutes\n+projectCommentRoutes project_handle = CommentRoutes\n+    { comment_route_add_tag   = ProjectCommentAddTagR   project_handle\n+    , comment_route_approve   = ApproveProjectCommentR  project_handle\n+    , comment_route_close     = CloseProjectCommentR    project_handle\n+    , comment_route_delete    = DeleteProjectCommentR   project_handle\n+    , comment_route_edit      = EditProjectCommentR     project_handle\n+    , comment_route_flag      = FlagProjectCommentR     project_handle\n+    , comment_route_permalink = ProjectCommentR         project_handle\n+    , comment_route_reply     = ReplyProjectCommentR    project_handle\n+    , comment_route_rethread  = RethreadProjectCommentR project_handle\n+    , comment_route_retract   = RetractProjectCommentR  project_handle\n+    , comment_route_tag       = ProjectCommentTagR      project_handle\n+    }\n+\n+wikiPageCommentRoutes :: Text -> Text -> CommentRoutes\n+wikiPageCommentRoutes project_handle target = CommentRoutes\n+    { comment_route_add_tag   = WikiCommentAddTagR   project_handle target\n+    , comment_route_approve   = ApproveWikiCommentR  project_handle target\n+    , comment_route_close     = CloseWikiCommentR    project_handle target\n+    , comment_route_delete    = DeleteWikiCommentR   project_handle target\n+    , comment_route_edit      = EditWikiCommentR     project_handle target\n+    , comment_route_flag      = FlagWikiCommentR     project_handle target\n+    , comment_route_permalink = WikiCommentR         project_handle target\n+    , comment_route_reply     = ReplyWikiCommentR    project_handle target\n+    , comment_route_rethread  = RethreadWikiCommentR project_handle target\n+    , comment_route_retract   = RetractWikiCommentR  project_handle target\n+    , comment_route_tag       = WikiCommentTagR      project_handle target\n+    }\ndiff --git a/Model/Comment/Sql.hs b/Model/Comment/Sql.hs\nindex 6ff6c4c..af65763 100644\n--- a/Model/Comment/Sql.hs\n+++ b/Model/Comment/Sql.hs\n@@ -2,11 +2,13 @@ module Model.Comment.Sql where\n \n import Import\n \n-import Model.User.Sql (exprIsModerator)\n+import Model.User.Sql\n \n-exprClosed, exprOpen :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)\n-exprClosed c = c ^. CommentId `in_`   exprClosedCommentIds\n-exprOpen   c = c ^. CommentId `notIn` exprClosedCommentIds\n+type ExprCommentCond = SqlExpr (Entity Comment) -> SqlExpr (Value Bool)\n+\n+exprCommentClosed, exprCommentOpen :: ExprCommentCond\n+exprCommentClosed c = c ^. CommentId `in_`   exprClosedCommentIds\n+exprCommentOpen   c = c ^. CommentId `notIn` exprClosedCommentIds\n \n exprClosedCommentIds :: SqlExpr (ValueList CommentId)\n exprClosedCommentIds =\n@@ -15,39 +17,19 @@ exprClosedCommentIds =\n     return (cl ^. CommentClosureComment)\n \n -- | Comment is root?\n-exprRoot :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)\n-exprRoot c = isNothing (c ^. CommentParent)\n+exprCommentIsRoot :: ExprCommentCond\n+exprCommentIsRoot c = isNothing (c ^. CommentParent)\n \n -- | Comment on this Discussion?\n-exprOnDiscussion :: DiscussionId -> SqlExpr (Entity Comment) -> SqlExpr (Value Bool)\n-exprOnDiscussion discussion_id c = c ^. CommentDiscussion ==. val discussion_id\n+exprCommentOnDiscussion :: DiscussionId -> ExprCommentCond\n+exprCommentOnDiscussion discussion_id c = c ^. CommentDiscussion ==. val discussion_id\n \n -- | Comment on this WikiPage?\n exprCommentOnWikiPage :: SqlExpr (Entity Comment) -> SqlExpr (Entity WikiPage) -> SqlExpr (Value Bool)\n exprCommentOnWikiPage c wp = c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion\n \n--- | SQL expression to filter a comment based on "permissions", as follows:\n---    If moderator, show all.\n---    If logged in, show all approved (hiding flagged), plus own comments (unapproved + flagged).\n---    If not logged in, show all approved (hiding flagged).\n---    No matter what, hide rethreaded comments (they've essentially been replaced).\n---\n--- The logic here is DUPLICATED (in Haskell land) in Handler.Wiki.Comment.checkCommentPage\n--- (because that function only fetches the root comment via Database.Persist.get) - all\n--- changes here must be reflected there, too!\n-exprPermissionFilter :: Maybe UserId\n-                     -> SqlExpr (Value ProjectId)\n-                     -> SqlExpr (Entity Comment)\n-                     -> SqlExpr (Value Bool)\n-exprPermissionFilter muser_id project_id c = exprNotRethreaded c &&. permissionFilter\n-  where\n-    permissionFilter :: SqlExpr (Value Bool)\n-    permissionFilter = case muser_id of\n-        Just user_id -> exprApprovedAndNotFlagged c ||. exprPostedBy user_id c ||. exprIsModerator user_id project_id\n-        Nothing      -> exprApprovedAndNotFlagged c\n-\n-exprNotRethreaded :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)\n-exprNotRethreaded c = c ^. CommentId `notIn` rethreadedCommentIds\n+exprCommentNotRethreaded :: ExprCommentCond\n+exprCommentNotRethreaded c = c ^. CommentId `notIn` rethreadedCommentIds\n   where\n     rethreadedCommentIds :: SqlExpr (ValueList CommentId)\n     rethreadedCommentIds =\n@@ -55,14 +37,14 @@ exprNotRethreaded c = c ^. CommentId `notIn` rethreadedCommentIds\n         from $ \\r ->\n         return (r ^. RethreadOldComment)\n \n-exprApproved :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)\n-exprApproved = not_ . exprUnapproved\n+exprCommentApproved :: ExprCommentCond\n+exprCommentApproved = not_ . exprCommentUnapproved\n \n-exprUnapproved :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)\n-exprUnapproved c = isNothing (c ^. CommentModeratedTs)\n+exprCommentUnapproved :: ExprCommentCond\n+exprCommentUnapproved c = isNothing (c ^. CommentApprovedTs)\n \n-exprNotFlagged :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)\n-exprNotFlagged c = c ^. CommentId `notIn` flaggedCommentIds\n+exprCommentFlagged :: ExprCommentCond\n+exprCommentFlagged c = c ^. CommentId `in_` flaggedCommentIds\n   where\n     flaggedCommentIds :: SqlExpr (ValueList CommentId)\n     flaggedCommentIds =\n@@ -70,33 +52,46 @@ exprNotFlagged c = c ^. CommentId `notIn` flaggedCommentIds\n         from $ \\cf ->\n         return (cf ^. CommentFlaggingComment)\n \n-exprApprovedAndNotFlagged :: SqlExpr (Entity Comment) -> SqlExpr (Value Bool)\n-exprApprovedAndNotFlagged c = exprApproved c &&. exprNotFlagged c\n-\n-exprPostedBy :: UserId -> SqlExpr (Entity Comment) -> SqlExpr (Value Bool)\n-exprPostedBy user_id c = c ^. CommentUser ==. val user_id\n+exprCommentPostedBy :: UserId -> ExprCommentCond\n+exprCommentPostedBy user_id c = c ^. CommentUser ==. val user_id\n \n-exprCommentViewedBy :: UserId -> SqlExpr (Entity Comment) -> SqlExpr (Value Bool)\n+exprCommentViewedBy :: UserId -> ExprCommentCond\n exprCommentViewedBy user_id c = c ^. CommentId `in_`\n     (subList_select $\n      from $ \\vc -> do\n      where_ (vc ^. ViewCommentUser ==. val user_id)\n      return (vc ^. ViewCommentComment))\n \n-querAncestors :: CommentId -> SqlQuery (SqlExpr (Value CommentId))\n-querAncestors comment_id =\n+-- | SQL expression to filter a Comment (somewhere) on a Project based on "permissions", as follows:\n+--    If moderator, show all.\n+--    If logged in, show all approved (hiding flagged), plus own comments (unapproved + flagged).\n+--    If not logged in, show all approved (hiding flagged).\n+--    No matter what, hide rethreaded comments (they've essentially been replaced).\n+exprCommentProjectPermissionFilter :: Maybe UserId -> SqlExpr (Value ProjectId) -> ExprCommentCond\n+exprCommentProjectPermissionFilter muser_id project_id c = exprCommentNotRethreaded c &&. permissionFilter\n+  where\n+    permissionFilter :: SqlExpr (Value Bool)\n+    permissionFilter = case muser_id of\n+        Just user_id -> approvedAndNotFlagged ||. exprCommentPostedBy user_id c ||. exprUserIsModerator user_id project_id\n+        Nothing      -> approvedAndNotFlagged\n+\n+    approvedAndNotFlagged :: SqlExpr (Value Bool)\n+    approvedAndNotFlagged = exprCommentApproved c &&. not_ (exprCommentFlagged c)\n+\n+querCommentAncestors :: CommentId -> SqlQuery (SqlExpr (Value CommentId))\n+querCommentAncestors comment_id =\n     from $ \\ca -> do\n     where_ (ca ^. CommentAncestorComment ==. val comment_id)\n     return (ca ^. CommentAncestorAncestor)\n \n-querDescendants :: CommentId -> SqlQuery (SqlExpr (Value CommentId))\n-querDescendants comment_id =\n+querCommentDescendants :: CommentId -> SqlQuery (SqlExpr (Value CommentId))\n+querCommentDescendants comment_id =\n     from $ \\ca -> do\n     where_ (ca ^. CommentAncestorAncestor ==. val comment_id)\n     return (ca ^. CommentAncestorComment)\n \n-querAllDescendants :: [CommentId] -> SqlQuery (SqlExpr (Value CommentId))\n-querAllDescendants comment_ids =\n+querCommentsDescendants :: [CommentId] -> SqlQuery (SqlExpr (Value CommentId))\n+querCommentsDescendants comment_ids =\n     from $ \\ca -> do\n     where_ (ca ^. CommentAncestorAncestor `in_` valList comment_ids)\n     return (ca ^. CommentAncestorComment)\ndiff --git a/Model/Discussion.hs b/Model/Discussion.hs\nindex 78af03b..4009b7e 100644\n--- a/Model/Discussion.hs\n+++ b/Model/Discussion.hs\n@@ -1,9 +1,36 @@\n module Model.Discussion\n-    ( fetchDiscussionWikiPagesInDB\n+    ( createDiscussionDB\n+    , fetchDiscussionClosedRootCommentsDB\n+    , fetchDiscussionProjectDB\n+    , fetchDiscussionRootCommentsDB\n+    , fetchDiscussionWikiPage\n+    , fetchDiscussionWikiPagesInDB\n     ) where\n \n import Import\n \n+import Model.Comment.Sql\n+import Control.Monad.Trans.Maybe\n+\n+-- | Get all open root Comments on a Discussion.\n+fetchDiscussionRootCommentsDB :: DiscussionId -> ExprCommentCond -> DB [Entity Comment]\n+fetchDiscussionRootCommentsDB = fetchRootComments exprCommentOpen\n+\n+-- | Get all closed root Comments on a Discussion.\n+fetchDiscussionClosedRootCommentsDB :: DiscussionId -> ExprCommentCond -> DB [Entity Comment]\n+fetchDiscussionClosedRootCommentsDB = fetchRootComments exprCommentClosed\n+\n+fetchRootComments :: ExprCommentCond -> DiscussionId -> ExprCommentCond -> DB [Entity Comment]\n+fetchRootComments open_or_closed discussion_id has_permission =\n+    select $\n+    from $ \\c -> do\n+    where_ $\n+        exprCommentOnDiscussion discussion_id c &&.\n+        exprCommentIsRoot c &&.\n+        open_or_closed c &&.\n+        has_permission c\n+    return c\n+\n -- | Given a list of DiscussionId, fetch the discussions which are WikiPages.\n fetchDiscussionWikiPagesInDB :: [DiscussionId] -> DB [Entity WikiPage]\n fetchDiscussionWikiPagesInDB discussion_ids =\n@@ -11,3 +38,27 @@ fetchDiscussionWikiPagesInDB discussion_ids =\n     from $ \\wp -> do\n     where_ (wp ^. WikiPageDiscussion `in_` valList discussion_ids)\n     return wp\n+\n+-- | Fetch the Project this Discussion is associated with (if any).\n+-- TODO(mitchell): Does this require constant attention, as we expand\n+-- discussions?\n+fetchDiscussionProjectDB :: DiscussionId -> DB (Maybe ProjectId)\n+fetchDiscussionProjectDB discussion_id = runMaybeT $\n+    -- From a list of possible ways to find a ProjectId from a DiscussionId, find the Project (maybe).\n+    foldr (mplus . f) mzero\n+        -- add more functions here as necessary\n+        [(fetchDiscussionWikiPage, wikiPageProject)]\n+  where\n+    -- f :: (DiscussionId -> DB (Maybe (Entity a)), a -> ProjectId) -> MaybeT DB (Entity Project)\n+    f (action, project_id_getter) = project_id_getter . entityVal <$> MaybeT (action discussion_id)\n+\n+-- | Fetch the WikiPage this Discussion is on with (if any).\n+fetchDiscussionWikiPage :: DiscussionId -> DB (Maybe (Entity WikiPage))\n+fetchDiscussionWikiPage discussion_id = fmap listToMaybe $\n+    select $\n+    from $ \\wp -> do\n+    where_ (wp ^. WikiPageDiscussion ==. val discussion_id)\n+    return wp\n+\n+createDiscussionDB :: DB DiscussionId\n+createDiscussionDB = insert (Discussion 0)\ndiff --git a/Model/Issue.hs b/Model/Issue.hs\nindex f960602..4c4ab58 100644\n--- a/Model/Issue.hs\n+++ b/Model/Issue.hs\n@@ -6,16 +6,13 @@ import Import\n \n import           Data.Filter\n import           Data.Order\n-import           Model.AnnotatedTag\n-import           Model.Ticket\n-import           Widgets.Tag        (pickForegroundColor, tagWidget)\n+import           Widgets.Tag (pickForegroundColor)\n \n import qualified Data.Set           as S\n import qualified Data.Text          as T\n import qualified Github.Issues      as GH\n import           Numeric            (readHex)\n import           Text.Printf\n-import           Yesod.Markdown     (unMarkdown)\n \n -- An Issue abstracts a Snowdrift ticket, Github issue, etc.\n class Issue a where\n@@ -34,42 +31,6 @@ instance Issue SomeIssue where\n     issueFilterable (SomeIssue k) = k issueFilterable\n     issueOrderable  (SomeIssue k) = k issueOrderable\n \n-instance Issue AnnotatedTicket where\n-    issueWidget (AnnotatedTicket project_handle ticket_id ticket page _ tags) =\n-        [whamlet|\n-          <tr>\n-            <td>\n-              <a href="@{DiscussCommentR project_handle (wikiPageTarget page) (ticketComment ticket)}">\n-                SD-#{toPathPiece ticket_id}\n-            <td>\n-              #{ticketName ticket}\n-            <td>\n-              $forall tag <- tags\n-                ^{tagWidget tag}\n-        |]\n-    issueFilterable = ticketToFilterable\n-    issueOrderable = ticketToOrderable\n-\n-ticketToFilterable :: AnnotatedTicket -> Filterable\n-ticketToFilterable (AnnotatedTicket _ _ ticket _ comment tags) = Filterable has_tag get_named_ts search_literal\n-  where\n-    has_tag t = any (\\ at -> atName at == t && atScore at > 0) tags\n-\n-    get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket\n-    get_named_ts "LAST UPDATED" = S.singleton $ ticketUpdatedTs ticket\n-    get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name\n-\n-    search_literal str = uncurry ((||) `on` not . null . T.breakOnAll str) (ticketName ticket, unMarkdown $ commentText comment)\n-\n-ticketToOrderable :: AnnotatedTicket -> Orderable\n-ticketToOrderable (AnnotatedTicket _ _ ticket _ comment tags) = Orderable has_tag get_named_ts search_literal\n-  where\n-    has_tag t = elem t $ map atName tags\n-    get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket\n-    get_named_ts "LAST UPDATED" = S.singleton $ ticketUpdatedTs ticket\n-    get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name\n-    search_literal str = uncurry ((||) `on` not . null . T.breakOnAll str) (ticketName ticket, unMarkdown $ commentText comment)\n-\n instance Issue GH.Issue where\n     issueWidget github_issue =\n         [whamlet|\ndiff --git a/Model/Markdown.hs b/Model/Markdown.hs\nindex 39937d2..bb3f993 100644\n--- a/Model/Markdown.hs\n+++ b/Model/Markdown.hs\n@@ -75,16 +75,22 @@ linkTickets line' = do\n \n \n renderMarkdown :: Text -> Markdown -> Handler Html\n-renderMarkdown project (Markdown markdown) = do\n+renderMarkdown = renderMarkdownWith return\n+\n+renderMarkdownWith :: (Text -> Handler Text) -> Text -> Markdown -> Handler Html\n+renderMarkdownWith transform project (Markdown markdown) = do\n     let ls = T.lines markdown\n \n-    ls' <- mapM (linkTickets . fixLinks project) ls\n+    ls' <- mapM (transform <=< linkTickets . fixLinks project) ls\n \n     return $ markdownToHtml $ Markdown $ T.unlines ls'\n \n \n markdownWidget :: Text -> Markdown -> Widget\n-markdownWidget project markdown = do\n-    rendered <- handlerToWidget $ renderMarkdown project markdown\n+markdownWidget = markdownWidgetWith return\n+\n+markdownWidgetWith :: (Text -> Handler Text) -> Text -> Markdown -> Widget\n+markdownWidgetWith transform project markdown = do\n+    rendered <- handlerToWidget $ renderMarkdownWith transform project markdown\n     toWidget rendered\n \ndiff --git a/Model/Message.hs b/Model/Message.hs\ndeleted file mode 100644\nindex e911ad7..0000000\n--- a/Model/Message.hs\n+++ /dev/null\n@@ -1,33 +0,0 @@\n-module Model.Message\n-    ( insertMessage\n-    , insertMessage_\n-    , module Model.Message.Internal\n-    ) where\n-\n-import Import\n-import Model.Message.Internal\n-\n-import Control.Monad.Writer.Strict (tell)\n-\n-insertMessage :: MessageType\n-              -> Maybe ProjectId\n-              -> Maybe UserId\n-              -> Maybe UserId\n-              -> Markdown\n-              -> Bool\n-              -> SDB MessageId\n-insertMessage message_type mproject_id mfrom mto content is_automated = do\n-    now <- liftIO getCurrentTime\n-    let message = Message message_type mproject_id now mfrom mto content is_automated\n-    message_id <- lift (insert message)\n-    tell [EMessageSent message_id message]\n-    return message_id\n-\n-insertMessage_ :: MessageType\n-               -> Maybe ProjectId\n-               -> Maybe UserId\n-               -> Maybe UserId\n-               -> Markdown\n-               -> Bool\n-               -> SDB ()\n-insertMessage_ a b c d e f = void $ insertMessage a b c d e f\ndiff --git a/Model/Message/Internal.hs b/Model/Message/Internal.hs\ndeleted file mode 100644\nindex e7f0377..0000000\n--- a/Model/Message/Internal.hs\n+++ /dev/null\n@@ -1,32 +0,0 @@\n-module Model.Message.Internal where\n-\n-import Prelude\n-\n-import Database.Persist.TH\n-import Data.Text (Text)\n-\n-data MessageType\n-    = MessageDirect     -- Direct message (can't be ignored)\n-    | MessageBalanceLow -- Balance low (can't be ignored)\n-    | MessageReply      -- Reply to a comment made.\n-    | MessageNewProject\n-    -- Project scope\n-    | MessageNewPledger\n-    | MessageNewPage\n-    deriving (Eq, Read, Show)\n-derivePersistField "MessageType"\n-\n-showMessageType :: MessageType -> Text\n-showMessageType MessageDirect     = "Snowdrift direct messages"\n-showMessageType MessageBalanceLow = "Balance low"\n-showMessageType MessageReply      = "Replies to my comments"\n-showMessageType MessageNewProject = "New project sign-ups"\n-showMessageType MessageNewPledger = "New pledgers"\n-showMessageType MessageNewPage    = "New Wiki pages"\n-\n-data MessageDelivery\n-    = DeliverInternal     -- Only send internal Snowdrift messages.\n-    | DeliverEmail        -- Send email in addition to internal messages.\n-    | DeliverEmailDigest  -- Send email digest in addition to internal messages (sent immediately)\n-    deriving (Read, Show)\n-derivePersistField "MessageDelivery"\ndiff --git a/Model/Notification.hs b/Model/Notification.hs\nnew file mode 100644\nindex 0000000..d3eb97b\n--- /dev/null\n+++ b/Model/Notification.hs\n@@ -0,0 +1,31 @@\n+module Model.Notification\n+    ( archiveNotificationDB\n+    , sendNotificationDB\n+    , sendNotificationDB_\n+    , module Model.Notification.Internal\n+    ) where\n+\n+import Import\n+\n+import Model.Notification.Internal\n+\n+import Control.Monad.Writer.Strict (tell)\n+\n+-- | Archive a notification.\n+archiveNotificationDB :: NotificationId -> DB ()\n+archiveNotificationDB notif_id =\n+    update $ \\n -> do\n+    set n [NotificationArchived =. val True]\n+    where_ (n ^. NotificationId ==. val notif_id)\n+\n+-- | Send a notification to a user.\n+sendNotificationDB :: NotificationType -> UserId -> Maybe ProjectId -> Markdown -> SDB NotificationId\n+sendNotificationDB notif_type user_id mproject_id content = do\n+    now <- liftIO getCurrentTime\n+    let notif = Notification now notif_type user_id mproject_id content False\n+    notif_id <- lift (insert notif)\n+    tell [ENotificationSent notif_id notif]\n+    return notif_id\n+\n+sendNotificationDB_ :: NotificationType -> UserId -> Maybe ProjectId -> Markdown -> SDB ()\n+sendNotificationDB_ notif_type user_id mproject_id content = void (sendNotificationDB notif_type user_id mproject_id content)\ndiff --git a/Model/Notification/Internal.hs b/Model/Notification/Internal.hs\nnew file mode 100644\nindex 0000000..3c56909\n--- /dev/null\n+++ b/Model/Notification/Internal.hs\n@@ -0,0 +1,43 @@\n+module Model.Notification.Internal where\n+\n+import Prelude\n+\n+import Database.Persist.TH\n+import Data.Text (Text)\n+\n+data NotificationType\n+    = NotifWelcome\n+    -- User has become eligible for establishment.\n+    | NotifEligEstablish\n+    -- Balance low (can't be ignored)\n+    | NotifBalanceLow\n+    -- Alert moderators about an unapproved comment.\n+    -- These notifications are auto-deleted when the comment is approved.\n+    | NotifUnapprovedComment\n+    -- Reply to a comment made.\n+    | NotifReply\n+    -- Edit conflict.\n+    | NotifEditConflict\n+    -- Comment flagged.\n+    | NotifFlag\n+    -- Flagged comment was reposted.\n+    | NotifFlagRepost\n+    deriving (Eq, Read, Show)\n+derivePersistField "NotificationType"\n+\n+showNotificationType :: NotificationType -> Text\n+showNotificationType NotifWelcome           = "Snowdrift welcome message"\n+showNotificationType NotifEligEstablish     = "You have become eligible for establishment"\n+showNotificationType NotifUnapprovedComment = "Unapproved comments"\n+showNotificationType NotifBalanceLow        = "Balance low"\n+showNotificationType NotifReply             = "Replies to my comments"\n+showNotificationType NotifEditConflict      = "Edit conflict"\n+showNotificationType NotifFlag              = "A comment of yours was flagged"\n+showNotificationType NotifFlagRepost        = "A comment you flagged was edited and reposted"\n+\n+data NotificationDelivery\n+    = NotifDeliverInternal     -- Only send notifications.\n+    | NotifDeliverEmail        -- Send email in addition to notifications.\n+    | NotifDeliverEmailDigest  -- Send email digest in addition to notifications (sent immediately).\n+    deriving (Read, Show)\n+derivePersistField "NotificationDelivery"\ndiff --git a/Model/Project.hs b/Model/Project.hs\nindex 7596001..c60b8dd 100644\n--- a/Model/Project.hs\n+++ b/Model/Project.hs\n@@ -1,10 +1,24 @@\n module Model.Project\n     ( ProjectSummary(..)\n-    , fetchProjectCommentIdsDB\n-    , fetchProjectCommentsPostedOnWikiPagesBeforeDB\n+    , UpdateProject(..)\n+    , fetchAllProjectsDB\n+    , fetchProjectCommentsDB\n+    , fetchProjectCommentsBeforeDB\n+    , fetchProjectPendingCommentsBeforeDB\n+    , fetchProjectWikiPageCommentsBeforeDB\n+    , fetchProjectDeletedPledgesBeforeDB\n+    , fetchProjectNewPledgesBeforeDB\n+    , fetchProjectModeratorsDB\n+    , fetchProjectTeamMembersDB\n+    , fetchProjectTicketsDB\n+    , fetchProjectTaggedTicketsDB\n+    , fetchProjectUpdatedPledgesBeforeDB\n+    , fetchProjectVolunteerApplicationsDB\n     , fetchProjectWikiEditsBeforeDB\n+    , fetchProjectWikiPagesBeforeDB\n+    , fetchProjectWikiPageByNameDB\n+    , insertProjectPledgeDB\n     -- TODO(mitchell): rename all these... prefix fetch, suffix DB\n-    , getAllProjects\n     , getGithubIssues\n     , getProjectPages\n     , getProjectShares\n@@ -18,32 +32,120 @@ module Model.Project\n \n import Import\n \n+import           Data.Filter\n+import           Data.Order\n+import           Model.Comment\n import           Model.Comment.Sql\n import           Model.Currency\n+import           Model.Issue\n import           Model.Project.Sql\n-import           Model.WikiPage.Sql\n+import           Model.Tag\n+import           Model.User\n+import           Model.Wiki.Sql\n+import           Widgets.Tag\n \n+import           Control.Monad.Trans.Maybe    (MaybeT(..), runMaybeT)\n import           Control.Monad.Trans.Resource (MonadThrow)\n+import           Control.Monad.Writer.Strict  (tell)\n import           Control.Concurrent.Async     (Async, async, wait)\n import qualified Github.Data                  as GH\n import qualified Github.Issues                as GH\n+import qualified Data.Map                     as M\n+import qualified Data.Set                     as S\n import qualified Data.Text                    as T\n \n-data ProjectSummary =\n-    ProjectSummary\n-        { summaryName          :: Text\n-        , summaryProjectHandle :: Text\n-        , summaryUsers         :: UserCount\n-        , summaryShares        :: ShareCount\n-        , summaryShareCost     :: Milray\n-        }\n+--------------------------------------------------------------------------------\n+-- Types\n \n--- | Fetch all Comments made on this Project, somewhere (for now, just WikiPages)\n-fetchProjectCommentIdsDB :: ProjectId -> DB [CommentId]\n-fetchProjectCommentIdsDB = fetchProjectCommentIdsPostedOnWikiPagesDB\n+data ProjectSummary = ProjectSummary\n+    { summaryName          :: Text\n+    , summaryProjectHandle :: Text\n+    , summaryUsers         :: UserCount\n+    , summaryShares        :: ShareCount\n+    , summaryShareCost     :: Milray\n+    }\n \n-getAllProjects :: DB [Entity Project]\n-getAllProjects = select (from return)\n+data UpdateProject = UpdateProject\n+    { updateProjectName        :: Text\n+    , updateProjectDescription :: Markdown\n+    , updateProjectTags        :: [Text]\n+    , updateProjectGithubRepo  :: Maybe Text\n+    } deriving Show\n+\n+newtype TaggedTicket = TaggedTicket ((Entity Ticket), [AnnotatedTag])\n+\n+instance Issue TaggedTicket where\n+    issueWidget (TaggedTicket ((Entity ticket_id ticket),tags)) =\n+        [whamlet|\n+          <tr>\n+            <td>\n+              <a href="@{CommentDirectLinkR (ticketComment ticket)}">\n+                SD-#{toPathPiece ticket_id}\n+            <td>\n+              #{ticketName ticket}\n+            <td>\n+              $forall tag <- tags\n+                ^{tagWidget tag}\n+        |]\n+    issueFilterable = ticketToFilterable\n+    issueOrderable = ticketToOrderable\n+\n+ticketToFilterable :: TaggedTicket -> Filterable\n+ticketToFilterable (TaggedTicket (Entity _ ticket, tags)) = Filterable has_tag get_named_ts search_literal\n+  where\n+    has_tag t = any (\\tag -> annotTagName tag == t && annotTagScore tag > 0) tags\n+\n+    get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket\n+    get_named_ts "LAST UPDATED" = S.singleton $ ticketUpdatedTs ticket\n+    get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name\n+\n+    search_literal str = (not . null . T.breakOnAll str) (ticketName ticket)\n+\n+ticketToOrderable :: TaggedTicket -> Orderable\n+ticketToOrderable (TaggedTicket ((Entity _ ticket),tags)) = Orderable has_tag get_named_ts search_literal\n+  where\n+    has_tag t = elem t $ map annotTagName tags\n+    get_named_ts "CREATED" = S.singleton $ ticketCreatedTs ticket\n+    get_named_ts "LAST UPDATED" = S.singleton $ ticketUpdatedTs ticket\n+    get_named_ts name = error $ "Unrecognized time name " ++ T.unpack name\n+    search_literal str = (not . null . T.breakOnAll str) (ticketName ticket)\n+\n+--------------------------------------------------------------------------------\n+-- Database actions\n+\n+-- | Fetch all Comments made on this Project, somewhere.\n+fetchProjectCommentsDB :: ProjectId -> Maybe UserId -> DB [CommentId]\n+fetchProjectCommentsDB project_id muser_id = fmap (map unValue) $ select (querProjectCommentsDB project_id muser_id)\n+\n+fetchAllProjectsDB :: DB [Entity Project]\n+fetchAllProjectsDB = select (from return)\n+\n+insertProjectPledgeDB :: UserId\n+                      -> ProjectId\n+                      -> Int64\n+                      -> PledgeFormRenderedId\n+                      -> SDB ()\n+insertProjectPledgeDB user_id project_id shares pledge_render_id = do\n+    now <- liftIO getCurrentTime\n+    let shares_pledged = SharesPledged now user_id project_id shares pledge_render_id\n+    shares_pledged_id <- lift (insert shares_pledged)\n+    getBy (UniquePledge user_id project_id) >>= \\case\n+        Nothing -> do\n+            lift $ insert_ (Pledge now user_id project_id shares shares)\n+            tell [ENewPledge shares_pledged_id shares_pledged]\n+        Just (Entity pledge_id old_pledge) -> do\n+            if shares == 0\n+                then do\n+                    lift (deleteKey pledge_id)\n+                    tell [EDeletedPledge now user_id project_id (pledgeShares old_pledge)]\n+                else do\n+                    lift $\n+                        update $ \\p -> do\n+                        set p [ PledgeShares       =. val shares\n+                              , PledgeFundedShares =. val shares\n+                              ]\n+                        where_ (p ^. PledgeId ==. val pledge_id)\n+                    tell [EUpdatedPledge (pledgeShares old_pledge) shares_pledged_id shares_pledged]\n \n getGithubIssues :: Project -> Handler [GH.Issue]\n getGithubIssues project =\n@@ -153,25 +255,62 @@ getProjectWikiPages project_id =\n     orderBy [asc (wp ^. WikiPageTarget)]\n     return wp\n \n--- | Fetch all Comments posted on some Project's WikiPages before some time.\n-fetchProjectCommentsPostedOnWikiPagesBeforeDB :: ProjectId -> UTCTime -> DB [Entity Comment]\n-fetchProjectCommentsPostedOnWikiPagesBeforeDB project_id before =\n+fetchProjectCommentsBeforeDB :: ProjectId -> Maybe UserId -> UTCTime -> Int64 -> DB [Entity Comment]\n+fetchProjectCommentsBeforeDB project_id muser_id before lim =\n+    select $\n+    from $ \\(ecp `InnerJoin` c) -> do\n+    on_ (ecp ^. EventCommentPostedComment ==. c ^. CommentId)\n+    where_ $\n+        ecp ^. EventCommentPostedTs <=. val before &&.\n+        exprCommentProjectPermissionFilter muser_id (val project_id) c &&.\n+        c ^. CommentDiscussion ==. (sub_select $\n+                                    from $ \\p -> do\n+                                    where_ (p ^. ProjectId ==. val project_id)\n+                                    return (p ^. ProjectDiscussion))\n+    limit lim\n+    return c\n+\n+-- | Fetch all Comments posted on this Project's WikiPages before this time.\n+fetchProjectWikiPageCommentsBeforeDB :: ProjectId -> Maybe UserId -> UTCTime -> Int64 -> DB [Entity Comment]\n+fetchProjectWikiPageCommentsBeforeDB project_id muser_id before lim =\n     select $\n     from $ \\(ecp `InnerJoin` c `InnerJoin` wp) -> do\n     on_ (exprCommentOnWikiPage c wp)\n     on_ (ecp ^. EventCommentPostedComment ==. c ^. CommentId)\n     where_ $\n         ecp ^. EventCommentPostedTs <=. val before &&.\n-        exprWikiPageOnProject wp project_id\n+        exprWikiPageOnProject wp project_id &&.\n+        exprCommentProjectPermissionFilter muser_id (val project_id) c\n+    limit lim\n+    return c\n+\n+-- | Fetch all pending Comments made on a Project before this time.\n+fetchProjectPendingCommentsBeforeDB :: ProjectId -> Maybe UserId -> UTCTime -> Int64 -> DB [Entity Comment]\n+fetchProjectPendingCommentsBeforeDB project_id muser_id before lim =\n+    select $\n+    from $ \\(ecp `InnerJoin` c) -> do\n+    on_ (ecp ^. EventCommentPendingComment ==. c ^. CommentId)\n+    where_ $\n+        ecp ^. EventCommentPendingTs <=. val before &&.\n+        exprCommentProjectPermissionFilter muser_id (val project_id) c\n+    limit lim\n     return c\n \n--- | Fetch all CommentIds on some Project's WikiPages.\n-fetchProjectCommentIdsPostedOnWikiPagesDB :: ProjectId -> DB [CommentId]\n-fetchProjectCommentIdsPostedOnWikiPagesDB = fmap (map unValue) . select . querProjectCommentIdsPostedOnWikiPagesDB\n+-- | Fetch all WikiPages made on this Project before this time.\n+fetchProjectWikiPagesBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [Entity WikiPage]\n+fetchProjectWikiPagesBeforeDB project_id before lim =\n+    select $\n+    from $ \\(ewp `InnerJoin` wp) -> do\n+    on_ (ewp ^. EventWikiPageWikiPage ==. wp ^. WikiPageId)\n+    where_ $\n+        ewp ^. EventWikiPageTs <=. val before &&.\n+        exprWikiPageOnProject wp project_id\n+    limit lim\n+    return wp\n \n--- | Fetch all WikiEdits made on some Project.\n-fetchProjectWikiEditsBeforeDB :: ProjectId -> UTCTime -> DB [Entity WikiEdit]\n-fetchProjectWikiEditsBeforeDB project_id before =\n+-- | Fetch all WikiEdits made on this Project before this time.\n+fetchProjectWikiEditsBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [Entity WikiEdit]\n+fetchProjectWikiEditsBeforeDB project_id before lim =\n     select $\n     from $ \\(ewe `InnerJoin` we `InnerJoin` wp) -> do\n     on_ (wp ^. WikiPageId ==. we ^. WikiEditPage)\n@@ -179,4 +318,90 @@ fetchProjectWikiEditsBeforeDB project_id before =\n     where_ $\n         ewe ^. EventWikiEditTs <=. val before &&.\n         exprWikiPageOnProject wp project_id\n+    limit lim\n     return we\n+\n+-- | Fetch all new SharesPledged made on this Project before this time.\n+fetchProjectNewPledgesBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [Entity SharesPledged]\n+fetchProjectNewPledgesBeforeDB project_id before lim =\n+    select $\n+    from $ \\(enp `InnerJoin` sp) -> do\n+    on_ (enp ^. EventNewPledgeSharesPledged ==. sp ^. SharesPledgedId)\n+    where_ $\n+        enp ^. EventNewPledgeTs <=. val before &&.\n+        sp ^. SharesPledgedProject ==. val project_id\n+    limit lim\n+    return sp\n+\n+-- | Fetch all updated Pledges made on this Project before this time, along with the old number of shares.\n+fetchProjectUpdatedPledgesBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [(Int64, Entity SharesPledged)]\n+fetchProjectUpdatedPledgesBeforeDB project_id before lim = fmap (map (\\(Value n, p) -> (n, p))) $\n+    select $\n+    from $ \\(eup `InnerJoin` sp) -> do\n+    on_ (eup ^. EventUpdatedPledgeSharesPledged ==. sp ^. SharesPledgedId)\n+    where_ $\n+        eup ^. EventUpdatedPledgeTs <=. val before &&.\n+        sp ^. SharesPledgedProject ==. val project_id\n+    limit lim\n+    return (eup ^. EventUpdatedPledgeOldShares, sp)\n+\n+-- | Fetch all deleted pledge events made on this Project before this time.\n+fetchProjectDeletedPledgesBeforeDB :: ProjectId -> UTCTime -> Int64 -> DB [EventDeletedPledge]\n+fetchProjectDeletedPledgesBeforeDB project_id before lim = fmap (map entityVal) $\n+    select $\n+    from $ \\edp -> do\n+    where_ $\n+        edp ^. EventDeletedPledgeTs      <=. val before &&.\n+        edp ^. EventDeletedPledgeProject ==. val project_id\n+    limit lim\n+    return edp\n+\n+-- | Fetch this Project's team members.\n+fetchProjectTeamMembersDB :: ProjectId -> DB [UserId]\n+fetchProjectTeamMembersDB = fetchProjectRoleDB TeamMember\n+\n+fetchProjectModeratorsDB :: ProjectId -> DB [UserId]\n+fetchProjectModeratorsDB = fetchProjectRoleDB Moderator\n+\n+-- | Abstract fetching Project Admins, TeamMembers, etc. Not exported.\n+fetchProjectRoleDB :: Role -> ProjectId -> DB [UserId]\n+fetchProjectRoleDB role project_id = fmap (map unValue) $\n+    select $\n+    from $ \\pur -> do\n+    where_ $\n+        pur ^. ProjectUserRoleProject ==. val project_id &&.\n+        pur ^. ProjectUserRoleRole    ==. val role\n+    return (pur ^. ProjectUserRoleUser)\n+  --\n+-- | Fetch all Project VolunteerApplications.\n+fetchProjectVolunteerApplicationsDB :: ProjectId -> DB [Entity VolunteerApplication]\n+fetchProjectVolunteerApplicationsDB project_id =\n+    select $\n+    from $ \\va -> do\n+    where_ (va ^. VolunteerApplicationProject ==. val project_id)\n+    orderBy [desc (va ^. VolunteerApplicationCreatedTs)]\n+    return va\n+\n+-- | Fetch a WikiPage (maybe), given the Project handle and WikiPage target.\n+-- (Presumably, these Texts come from something like a rethread form,\n+-- where the user types in URLs manually).\n+fetchProjectWikiPageByNameDB :: Text -> Text -> DB (Maybe (Entity WikiPage))\n+fetchProjectWikiPageByNameDB project_handle target = runMaybeT $ do\n+    Entity project_id _ <- MaybeT (getBy (UniqueProjectHandle project_handle))\n+    MaybeT (getBy (UniqueWikiTarget project_id target))\n+\n+fetchProjectTicketsDB :: ProjectId -> Maybe UserId -> DB [Entity Ticket]\n+fetchProjectTicketsDB project_id muser_id =\n+    select $\n+    from $ \\(t `InnerJoin` c) -> do\n+    on_ (t ^. TicketComment ==. c ^. CommentId)\n+    where_ (c ^. CommentId `in_` subList_select (querProjectCommentsDB project_id muser_id))\n+    return t\n+\n+fetchProjectTaggedTicketsDB :: ProjectId -> Maybe UserId -> DB [TaggedTicket]\n+fetchProjectTaggedTicketsDB project_id muser_id = do\n+    tickets <- fetchProjectTicketsDB project_id muser_id\n+    annot_tags_map <- fetchCommentCommentTagsInDB (map (ticketComment . entityVal) tickets) >>= buildAnnotatedCommentTagsDB muser_id\n+    let tagTicket :: Entity Ticket -> TaggedTicket\n+        tagTicket t@(Entity _ ticket) = TaggedTicket (t, M.findWithDefault [] (ticketComment ticket) annot_tags_map)\n+    return (map tagTicket tickets)\ndiff --git a/Model/Project/Sql.hs b/Model/Project/Sql.hs\nindex d3f02b4..887e5cd 100644\n--- a/Model/Project/Sql.hs\n+++ b/Model/Project/Sql.hs\n@@ -3,11 +3,28 @@ module Model.Project.Sql where\n import Import\n \n import Model.Comment.Sql\n-import Model.WikiPage.Sql\n+import Model.Wiki.Sql\n \n-querProjectCommentIdsPostedOnWikiPagesDB :: ProjectId -> SqlQuery (SqlExpr (Value CommentId))\n-querProjectCommentIdsPostedOnWikiPagesDB project_id =\n+querProjectCommentsDB :: ProjectId -> Maybe UserId -> SqlQuery (SqlExpr (Value CommentId))\n+querProjectCommentsDB project_id muser_id =\n+    from $ \\c -> do\n+    -- Add more locations for Comments here as necessary.\n+    where_ (c ^. CommentId `in_` subList_select (querProjectCommentsOnWikiPagesDB project_id muser_id))\n+    return (c ^. CommentId)\n+\n+querProjectCommentsOnWikiPagesDB :: ProjectId -> Maybe UserId -> SqlQuery (SqlExpr (Value CommentId))\n+querProjectCommentsOnWikiPagesDB project_id muser_id =\n     from $ \\(c `InnerJoin` wp) -> do\n     on_ (exprCommentOnWikiPage c wp)\n-    where_ (exprWikiPageOnProject wp project_id)\n+    where_ $\n+        exprWikiPageOnProject wp project_id &&.\n+        exprCommentProjectPermissionFilter muser_id (val project_id) c\n     return (c ^. CommentId)\n+\n+-- | Query that returns all WikiEdits made on any WikiPage on this Project\n+querProjectWikiEdits :: ProjectId -> SqlQuery (SqlExpr (Value WikiEditId))\n+querProjectWikiEdits project_id =\n+    from $ \\(wp `InnerJoin` we) -> do\n+    on_ (wp ^. WikiPageId ==. we ^. WikiEditPage)\n+    where_ (exprWikiPageOnProject wp project_id)\n+    return (we ^. WikiEditId)\ndiff --git a/Model/Role/Internal.hs b/Model/Role/Internal.hs\nindex 9be71fe..5b2149f 100644\n--- a/Model/Role/Internal.hs\n+++ b/Model/Role/Internal.hs\n@@ -1,15 +1,11 @@\n-\n module Model.Role.Internal where\n \n import Prelude\n \n-import Database.Persist.TH\n-\n-import Web.PathPieces\n-\n import Data.Text as T\n-\n+import Database.Persist.TH\n import Debug.Trace\n+import Web.PathPieces\n \n data Role\n     = TeamMember\ndiff --git a/Model/Shares.hs b/Model/Shares.hs\nindex e32bd05..9d6cf2c 100644\n--- a/Model/Shares.hs\n+++ b/Model/Shares.hs\n@@ -39,14 +39,14 @@ pledgeField project_id = Field\n             case mv of\n                 Nothing -> return $ Left $ SomeMessage MsgValueRequired\n                 Just v -> return $ parseValue v\n-            \n+\n         | otherwise = return $ parseValue x\n \n-    parseValue v = \n+    parseValue v =\n         case T.decimal v of\n             Right (a, "") -> Right $ Just $ SharesPurchaseOrder a\n             _ -> Left $ SomeMessage $ MsgInvalidInteger v\n-            \n+\n     view ident name attrs v req = do\n         now <- liftIO getCurrentTime\n         list <- handlerToWidget get_list\n@@ -65,7 +65,7 @@ pledgeField project_id = Field\n                 $forall amount <- list\n                     <input id="#{ident}-#{amount}" .radio-inline name="#{name}" *{attrs} type="radio" :req:required value="#{amount}" :amount == value:checked>\n                     #{amount}\n-                    \n+\n                 <div>\n                     <input id="#{ident}-other" .radio-inline name="#{name}" *{attrs} type="radio" :req:required value="#{name}-other" :not hasValue:checked>other:&nbsp;\n                     <input id="#{ident}-other-val" .form-inline style="width : 2.5em; text-align : center" name="#{name}-other" *{attrs} type="text" value="#{otherValue}">\n@@ -115,7 +115,7 @@ pledgeField project_id = Field\n \n \n \n-    \n+\n \n pledgeForm :: ProjectId -> Form SharesPurchaseOrder\n pledgeForm project_id extra = do\n@@ -128,7 +128,7 @@ pledgeForm project_id extra = do\n                     &&. pledge ^. PledgeUser ==. val user_id\n                 return $ pledge ^. PledgeShares\n \n-    \n+\n     (result, pledge_view) <- mreq (pledgeField project_id) "" (if shares > 0 then Just (SharesPurchaseOrder shares) else Nothing)\n \n     let view = [whamlet|\ndiff --git a/Model/SnowdriftEvent.hs b/Model/SnowdriftEvent.hs\nindex 922c99b..fed7d6f 100644\n--- a/Model/SnowdriftEvent.hs\n+++ b/Model/SnowdriftEvent.hs\n@@ -1,5 +1,6 @@\n module Model.SnowdriftEvent\n     ( snowdriftEventNewestToOldest\n+    , snowdriftEventTime\n     ) where\n \n import Import\n@@ -8,7 +9,11 @@ snowdriftEventNewestToOldest :: SnowdriftEvent -> SnowdriftEvent -> Ordering\n snowdriftEventNewestToOldest x y  = compare (snowdriftEventTime y) (snowdriftEventTime x)\n \n snowdriftEventTime :: SnowdriftEvent -> UTCTime\n-snowdriftEventTime (ECommentPosted  _ Comment{..})  = fromMaybe commentCreatedTs commentModeratedTs\n-snowdriftEventTime (ECommentPending _ Comment{..})  = commentCreatedTs\n-snowdriftEventTime (EMessageSent    _ Message{..})  = messageCreatedTs\n-snowdriftEventTime (EWikiEdit       _ WikiEdit{..}) = wikiEditTs\n+snowdriftEventTime (ECommentPosted _ Comment{..})         = fromMaybe commentCreatedTs commentApprovedTs\n+snowdriftEventTime (ECommentPending _ Comment{..})        = commentCreatedTs\n+snowdriftEventTime (ENotificationSent _ Notification{..}) = notificationCreatedTs\n+snowdriftEventTime (EWikiEdit _ WikiEdit{..})             = wikiEditTs\n+snowdriftEventTime (EWikiPage _ WikiPage{..})             = wikiPageCreatedTs\n+snowdriftEventTime (ENewPledge _ SharesPledged{..})       = sharesPledgedTs\n+snowdriftEventTime (EUpdatedPledge _ _ SharesPledged{..}) = sharesPledgedTs\n+snowdriftEventTime (EDeletedPledge ts _ _ _)              = ts\ndiff --git a/Model/SnowdriftEvent/Internal.hs b/Model/SnowdriftEvent/Internal.hs\nindex b7f8956..6506e83 100644\n--- a/Model/SnowdriftEvent/Internal.hs\n+++ b/Model/SnowdriftEvent/Internal.hs\n@@ -4,9 +4,24 @@ module Model.SnowdriftEvent.Internal\n \n import Model\n \n+import Data.Int (Int64)\n+import Data.Time (UTCTime)\n+\n -- A sum type of all events, each of which have their own database table.\n data SnowdriftEvent\n-    = ECommentPosted  CommentId Comment   -- Comment approved.\n-    | ECommentPending CommentId Comment   -- Comment unapproved (pending approval).\n-    | EMessageSent    MessageId Message\n-    | EWikiEdit       WikiEditId WikiEdit\n+    -- Comment approved.\n+    = ECommentPosted  CommentId Comment\n+    -- Comment unapproved (pending approval).\n+    | ECommentPending CommentId Comment\n+    | ENotificationSent NotificationId Notification\n+    -- New WikiEdit made.\n+    | EWikiEdit WikiEditId WikiEdit\n+    -- New WikiPage posted.\n+    | EWikiPage WikiPageId WikiPage\n+    -- New pledge.\n+    | ENewPledge SharesPledgedId SharesPledged\n+    -- Pledge that has changed in value.\n+    | EUpdatedPledge Int64 {- old shares -}\n+                     SharesPledgedId SharesPledged {- new pledge info -}\n+    -- Deleted pledge.\n+    | EDeletedPledge UTCTime UserId ProjectId Int64\ndiff --git a/Model/Tag.hs b/Model/Tag.hs\nindex ea33d8a..f7927ea 100644\n--- a/Model/Tag.hs\n+++ b/Model/Tag.hs\n@@ -1,15 +1,80 @@\n module Model.Tag\n     ( TagMap\n-    , getAllTags\n-    , getAllTagsMap\n+    , AnnotatedTag(..)\n+    , annotTagName\n+    , annotTagScore\n+    , annotTagScoreString\n+    , annotTagUserScore\n+    , sortAnnotTagsByName\n+    , sortAnnotTagsByScore\n+    , fetchAllTagsDB\n+    , fetchTagsInDB\n+    , fetchTagColorsDB\n+    , fetchDefaultTagColorsDB\n     ) where\n \n import Import\n \n+import           Data.List   (sortBy)\n+import qualified Data.Map    as M\n+import qualified Data.List   as L\n+import           Text.Printf\n+\n type TagMap = Map TagId Tag\n \n-getAllTagsMap :: DB TagMap\n-getAllTagsMap = entitiesMap <$> getAllTags\n+fetchAllTagsDB :: DB [Entity Tag]\n+fetchAllTagsDB = select (from return)\n+\n+fetchTagsInDB :: [TagId] -> DB [Entity Tag]\n+fetchTagsInDB tag_ids =\n+    select $\n+        from $ \\t -> do\n+        where_ (t ^. TagId `in_` valList tag_ids)\n+        return t\n+\n+fetchTagColorsDB :: UserId -> DB (Map TagId Color)\n+fetchTagColorsDB user_id = fmap go $\n+    select $\n+    from $ \\tc -> do\n+    where_ (tc ^. TagColorUser ==. val user_id)\n+    return tc\n+  where\n+    go :: [Entity TagColor] -> Map TagId Color\n+    go = M.fromList . map ((tagColorTag &&& Color . tagColorColor) . entityVal)\n+\n+fetchDefaultTagColorsDB :: DB (Map TagId Color)\n+fetchDefaultTagColorsDB = go <$> select (from return)\n+  where\n+    go :: [Entity DefaultTagColor] -> Map TagId Color\n+    go = M.fromList . map ((defaultTagColorTag &&& Color . defaultTagColorColor) . entityVal)\n+\n+-- | An tag 'annotated' with rendering information..\n+data AnnotatedTag = AnnotatedTag\n+    { annotTagTag       :: Entity Tag\n+    , annotTagUrl       :: Route App  -- ^ The route to POST to (for changing votes).\n+    , annotTagColor     :: Color\n+    , annotTagUserVotes :: [(Entity User, Int)]\n+    }\n+\n+annotTagName :: AnnotatedTag -> Text\n+annotTagName = tagName . entityVal . annotTagTag\n+\n+{- Scoring for voting on tags is something not currently presented\n+- on the site. We've discussed changing it. I (Aaron) prefer a 6-point\n+- range voting just like we proposed for the Bylaws instead of mimicking\n+- the pledge formula here. Final decisions haven't been made yet -}\n+\n+annotTagScore :: AnnotatedTag -> Double\n+annotTagScore = sum . map (\\ (_, x) -> if x == 0 then 0 else fromIntegral (signum x) * logBase 2 (1 + fromIntegral (abs x) :: Double)) . annotTagUserVotes\n+\n+annotTagUserScore :: AnnotatedTag -> UserId -> Maybe Int\n+annotTagUserScore at user_id = fmap snd $ L.find ((== user_id) . entityKey . fst) $ annotTagUserVotes at\n+\n+annotTagScoreString :: AnnotatedTag -> String\n+annotTagScoreString = printf "%.1f" . annotTagScore\n+\n+sortAnnotTagsByName :: [AnnotatedTag] -> [AnnotatedTag]\n+sortAnnotTagsByName = sortBy (compare `on` annotTagName)\n \n-getAllTags :: DB [Entity Tag]\n-getAllTags = select $ from (\\t -> return t)\n+sortAnnotTagsByScore :: [AnnotatedTag] -> [AnnotatedTag]\n+sortAnnotTagsByScore = sortBy (compare `on` annotTagScore)\ndiff --git a/Model/Ticket.hs b/Model/Ticket.hs\ndeleted file mode 100644\nindex baa453d..0000000\n--- a/Model/Ticket.hs\n+++ /dev/null\n@@ -1,52 +0,0 @@\n-module Model.Ticket where\n-\n-import Import\n-\n-import Model.AnnotatedTag\n-import Model.Comment      (getCommentTags)\n-\n-import qualified Data.Map as M\n-import qualified Data.Set as S\n-\n-data AnnotatedTicket = AnnotatedTicket Text TicketId Ticket WikiPage Comment [AnnotatedTag]\n-\n-getTickets :: ProjectId -> Text -> YDB [AnnotatedTicket]\n-getTickets project_id project_handle = do\n-    tickets_info <- getTicketsInfo\n-\n-    -- used_tags'tickets :: [(Set TagId, Map TagId Tag -> Handler AnnotatedTicket)]\n-    used_tags'tickets <-\n-        -- TODO: refactor this to avoid N+1 selects (for example, select all CommentTags where\n-        -- comment_id in comment_ids)\n-        forM tickets_info $ \\(Entity ticket_id ticket, Entity comment_id comment, Entity _ page) -> do\n-            used_tags <- map entityVal <$> getCommentTags comment_id\n-\n-            let t :: Map TagId Tag -> Handler AnnotatedTicket\n-                t tags_map = AnnotatedTicket project_handle ticket_id ticket page comment <$>\n-                    buildAnnotatedTags\n-                        tags_map\n-                        (CommentTagR project_handle (wikiPageTarget page) comment_id)\n-                        used_tags\n-\n-            return (S.fromList $ map commentTagTag used_tags, t)\n-\n-    tags_map <- M.fromList . map (entityKey &&& entityVal) <$>\n-        (select $\n-            from $ \\tag -> do\n-            where_ $ tag ^. TagId `in_` valList (S.toList . mconcat $ map fst used_tags'tickets)\n-            return tag)\n-\n-    mapM (\\(_, t) -> lift $ t tags_map) used_tags'tickets\n-  where\n-    -- Get all of this Project's Ticket/Comment/WikiPage tuples, for all open tickets.\n-    getTicketsInfo :: DB [(Entity Ticket, Entity Comment, Entity WikiPage)]\n-    getTicketsInfo =\n-        select $\n-        from $ \\(ticket `InnerJoin` comment `InnerJoin` page) -> do\n-        on_ (page ^. WikiPageDiscussion ==. comment ^. CommentDiscussion)\n-        on_ (comment ^. CommentId ==. ticket ^. TicketComment)\n-        where_ (page ^. WikiPageProject ==. val project_id &&.\n-                comment ^. CommentId `notIn` subList_select\n-                                                 (from $ \\closure ->\n-                                                  return $ closure ^. CommentClosureComment))\n-        return (ticket, comment, page)\ndiff --git a/Model/Transaction.hs b/Model/Transaction.hs\nindex 7794b0e..b43bbec 100644\n--- a/Model/Transaction.hs\n+++ b/Model/Transaction.hs\n@@ -24,11 +24,11 @@ renderOtherAccount is_credit transaction user_accounts project_accounts = do\n                 <a href="@{ProjectR (projectHandle project)}">\n                     #{projectName project}\n             |]\n-        \n+\n         (Nothing, Just (Entity user_id user)) ->\n             [hamlet|\n                 <a href="@{UserR user_id}">\n-                    #{userPrintName (Entity user_id user)}\n+                    #{userDisplayName (Entity user_id user)}\n             |]\n \n         (Nothing, Nothing) ->\ndiff --git a/Model/User.hs b/Model/User.hs\nindex b13336a..dc35f6d 100644\n--- a/Model/User.hs\n+++ b/Model/User.hs\n@@ -1,45 +1,65 @@\n module Model.User\n     ( UserMap\n-    , UserUpdate(..)\n-    , applyUserUpdate\n-    , canCurUserMakeEligible\n-    , canMakeEligible\n-    , eligEstablishUser\n-    , establishUser\n-    , fetchUserMessagePrefDB\n+    -- Utility functions\n+    , curUserIsEligibleEstablish\n+    , updateUserPreview\n+    , userCanAddTag\n+    , userCanCloseComment\n+    , userCanEditComment\n+    , userCanEditWikiPage\n+    , userIsEligibleEstablish\n+    , userIsEstablished\n+    , userIsUnestablished\n+    , userDisplayName\n+    -- Database actions\n+    , buildAnnotatedCommentTagsDB\n+    , eligEstablishUserDB\n+    , establishUserDB\n+    , fetchAllUserRolesDB\n+    , fetchCurUserRolesDB\n+    , fetchNumUnreadNotificationsDB\n+    , fetchNumUnviewedCommentsOnProjectWikiPagesDB\n+    , fetchNumUnviewedWikiEditsOnProjectDB\n+    , fetchUserArchivedNotificationsDB\n+    , fetchUserNotificationsDB\n+    , fetchUserNotificationPrefDB\n+    , fetchUserProjectsAndRolesDB\n+    , fetchUserRolesDB\n     , fetchUsersInDB\n-    -- TODO(mitchell): consistent naming scheme\n-    , getAllRoles\n-    , getCurUserRoles\n-    , getProjectsAndRoles\n-    , getRoles\n-    , hasRole\n-    , isCurUserEligibleEstablish\n-    , isCurUserProjectModerator\n-    , isEligibleEstablish\n-    , isEstablished\n-    , isProjectAdmin\n-    , isProjectAdmin'\n-    , isProjectAffiliated\n-    , isProjectModerator\n-    , isProjectModerator'\n-    , isProjectTeamMember\n-    , isProjectTeamMember'\n-    , updateUser\n-    , userPrintName\n+    , updateUserDB\n+    , userCanDeleteCommentDB\n+    , userHasRoleDB\n+    , userHasRolesAnyDB\n+    , userIsAffiliatedWithProjectDB\n+    , userIsProjectAdminDB\n+    , userIsProjectModeratorDB\n+    , userIsProjectTeamMemberDB\n+    , userIsWatchingProjectDB\n+    , userMaybeViewProjectCommentsDB\n+    , userReadNotificationsDB\n+    , userReadVolunteerApplicationsDB\n     , userUnwatchProjectDB\n-    , userWatchProjectDB\n-    , userWatchingProjectDB\n-    , userWidget\n     , userViewCommentsDB\n+    , userViewWikiEditsDB\n+    , userWatchProjectDB\n+    -- Unsorted\n+    , canCurUserMakeEligible\n+    , canMakeEligible\n     ) where\n \n import Import\n \n+\n+import Model.Comment\n import Model.Comment.Sql\n-import Model.Message\n+import Model.Notification\n import Model.Project.Sql\n+import Model.Tag\n+import Model.User.Internal\n+import Model.User.Sql\n+import Model.Wiki.Sql\n \n+import           Data.List      (sortBy)\n import qualified Data.Map       as M\n import qualified Data.Set       as S\n import qualified Data.Text      as T\n@@ -47,71 +67,88 @@ import           Yesod.Markdown (Markdown(..))\n \n type UserMap = Map UserId User\n \n-data UserUpdate =\n-    UserUpdate\n-        { userUpdateName               :: Maybe Text\n-        , userUpdateAvatar             :: Maybe Text\n-        , userUpdateIrcNick            :: Maybe Text\n-        , userUpdateBlurb              :: Maybe Markdown\n-        , userUpdateStatement          :: Maybe Markdown\n-        -- , userUpdateMessagePreferences :: Maybe [MessagePreference]\n-        }\n+--------------------------------------------------------------------------------\n+-- Utility functions\n \n-fetchUsersInDB :: [UserId] -> DB [Entity User]\n-fetchUsersInDB user_ids = selectList [UserId <-. user_ids] []\n+userCanAddTag :: User -> Bool\n+userCanAddTag = userIsEstablished\n \n-updateUser :: UserId -> UserUpdate -> DB ()\n-updateUser user_id UserUpdate{..} =\n-    update $ \\u -> do\n-    set u $ [ UserName               =. val userUpdateName\n-            , UserAvatar             =. val userUpdateAvatar\n-            , UserIrcNick            =. val userUpdateIrcNick\n-            , UserStatement          =. val userUpdateStatement\n-            , UserBlurb              =. val userUpdateBlurb\n-            -- , UserMessagePreferences =. val (fromMaybe [] userUpdateMessagePreferences)\n-            ]\n-    where_ (u ^. UserId ==. val user_id)\n+-- TODO: what should this be?\n+-- Aaron says: I think we should allow established to mark as closed,\n+-- but only *affiliated* OR the original poster should do so in one step,\n+-- otherwise, the marking of closed should require *moderator* confirmation…\n+-- We should also have a re-open function.\n+-- There are now comments discussing these things on the site.\n+userCanCloseComment :: User -> Bool\n+userCanCloseComment = userIsEstablished\n+\n+-- | Can this User edit this Comment?\n+userCanEditComment :: UserId -> Comment -> Bool\n+userCanEditComment user_id = (user_id ==) . commentUser\n+\n+userCanEditWikiPage :: User -> Bool\n+userCanEditWikiPage = userIsEstablished\n+\n+-- | Is the user established?\n+userIsEstablished :: User -> Bool\n+userIsEstablished = estIsEstablished . userEstablished\n \n-applyUserUpdate :: User -> UserUpdate -> User\n-applyUserUpdate user UserUpdate{..} = user\n+-- | Is the user eligible for establishment?\n+userIsEligibleEstablish :: User -> Bool\n+userIsEligibleEstablish = estIsEligible . userEstablished\n+\n+-- | Is the user unestablished?\n+userIsUnestablished :: User -> Bool\n+userIsUnestablished = estIsUnestablished . userEstablished\n+\n+-- | Is the current user eligible for establishment?\n+curUserIsEligibleEstablish :: Handler Bool\n+curUserIsEligibleEstablish = maybe False (userIsEligibleEstablish . entityVal) <$> maybeAuth\n+\n+-- | Get a User's public display name (defaults to userN if no name has been set).\n+userDisplayName :: Entity User -> Text\n+userDisplayName (Entity user_id user) = fromMaybe ("user" <> toPathPiece user_id) (userName user)\n+\n+-- | Apply a UserUpdate in memory, for preview. For this reason,\n+-- userUpdateNotificationPreferences doesn't need to be touched.\n+updateUserPreview :: UserUpdate -> User -> User\n+updateUserPreview UserUpdate{..} user = user\n     { userName               = userUpdateName\n     , userAvatar             = userUpdateAvatar\n     , userIrcNick            = userUpdateIrcNick\n     , userStatement          = userUpdateStatement\n     , userBlurb              = userUpdateBlurb\n-    -- , userMessagePreferences = fromMaybe [] userUpdateMessagePreferences\n     }\n \n-userPrintName :: Entity User -> Text\n-userPrintName (Entity user_id user) = fromMaybe ("user" <> toPathPiece user_id) (userName user)\n-\n-userWidget :: UserId -> Widget\n-userWidget user_id = do\n-    maybe_user <- handlerToWidget $ runDB $ get user_id\n-    case maybe_user of\n-        Nothing -> [whamlet|deleted user|]\n-        Just user ->\n-            [whamlet|\n-                <a href=@{UserR user_id}>\n-                    #{userPrintName (Entity user_id user)}\n-            |]\n-\n-isEstablished :: User -> Bool\n-isEstablished = estIsEstablished . userEstablished\n-\n-isEligibleEstablish :: User -> Bool\n-isEligibleEstablish = estIsEligible . userEstablished\n+--------------------------------------------------------------------------------\n+-- Database functions\n \n-isUnestablished :: User -> Bool\n-isUnestablished = estIsUnestablished . userEstablished\n+fetchUsersInDB :: [UserId] -> DB [Entity User]\n+fetchUsersInDB user_ids = selectList [UserId <-. user_ids] []\n \n-isCurUserEligibleEstablish :: Handler Bool\n-isCurUserEligibleEstablish = maybe False (isEligibleEstablish . entityVal) <$> maybeAuth\n+updateUserDB :: UserId -> UserUpdate -> DB ()\n+updateUserDB user_id UserUpdate{..} = do\n+    update $ \\u -> do\n+     set u $ [ UserName               =. val userUpdateName\n+             , UserAvatar             =. val userUpdateAvatar\n+             , UserIrcNick            =. val userUpdateIrcNick\n+             , UserStatement          =. val userUpdateStatement\n+             , UserBlurb              =. val userUpdateBlurb\n+             ]\n+     where_ (u ^. UserId ==. val user_id)\n+\n+    delete $\n+     from $ \\ump -> do\n+     where_ (ump ^. UserId ==. val user_id)\n+-- This stuff next two lines sets notification prefs, but not complete yet\n+-- with UI etc:\n+--  let new_prefs = map (uncurry (UserNotificationPref user_id)) userUpdateNotificationPreferences\n+--  void (insertMany new_prefs)\n \n -- | Establish a user, given their eligible-timestamp and reason for\n--- eligibility. Mark all unmoderated comments of theirs as moderated.\n-establishUser :: UserId -> UTCTime -> Text -> DB ()\n-establishUser user_id elig_time reason = do\n+-- eligibility. Mark all unapproved comments of theirs as approved.\n+establishUserDB :: UserId -> UTCTime -> Text -> DB ()\n+establishUserDB user_id elig_time reason = do\n     est_time <- liftIO getCurrentTime\n \n     let est = EstEstablished elig_time est_time reason\n@@ -124,39 +161,39 @@ establishUser user_id elig_time reason = do\n     approveUnapprovedComments :: UTCTime -> DB ()\n     approveUnapprovedComments est_time =\n         update $ \\c -> do\n-            set c [ CommentModeratedTs =. just (val est_time)\n-                  , CommentModeratedBy =. just (val user_id)\n+            set c [ CommentApprovedTs =. just (val est_time)\n+                  , CommentApprovedBy =. just (val user_id)\n                   ]\n             where_ $\n                 c ^. CommentUser ==. val user_id &&.\n-                exprUnapproved c\n+                exprCommentUnapproved c\n \n--- | Make a user eligible for establishment. Put a message in their inbox\n+-- | Make a user eligible for establishment. Put a notification in their inbox\n -- instructing them to read and accept the honor pledge.\n-eligEstablishUser :: UserId -> UserId -> Text -> SDB ()\n-eligEstablishUser establisher_id user_id reason = do\n+eligEstablishUserDB :: UserId -> UserId -> Text -> SDB ()\n+eligEstablishUserDB establisher_id user_id reason = do\n     elig_time <- liftIO getCurrentTime\n     let est = EstEligible elig_time reason\n     lift $\n         update $ \\u -> do\n-        set u [ UserEstablished =. val est ]\n+        set u [UserEstablished =. val est]\n         where_ (u ^. UserId ==. val user_id)\n \n     lift $ insert_ $ ManualEstablishment user_id establisher_id\n \n     snowdrift_id <- lift getSnowdriftId\n-    insertMessage_ MessageDirect (Just snowdrift_id) Nothing (Just user_id) message_text True\n+    sendNotificationDB_ NotifEligEstablish user_id (Just snowdrift_id) content\n   where\n-    message_text :: Markdown\n-    message_text = Markdown $ T.unlines\n+    content :: Markdown\n+    content = Markdown $ T.unlines\n         [ "You are now eligible to become an *established* user."\n         , ""\n         , "After you [accept the honor pledge](/honor-pledge), you can comment and take other actions on the site without moderation."\n         ]\n \n -- | Get a User's Roles in a Project.\n-getRoles :: UserId -> ProjectId -> DB [Role]\n-getRoles user_id project_id = fmap (map unValue) $\n+fetchUserRolesDB :: UserId -> ProjectId -> DB [Role]\n+fetchUserRolesDB user_id project_id = fmap (map unValue) $\n     select $\n         from $ \\r -> do\n         where_ (r ^. ProjectUserRoleProject ==. val project_id &&.\n@@ -164,26 +201,30 @@ getRoles user_id project_id = fmap (map unValue) $\n         return $ r ^. ProjectUserRoleRole\n \n -- | Get all of a User's Roles, across all Projects.\n-getAllRoles :: UserId -> DB [Role]\n-getAllRoles user_id = fmap unwrapValues $\n+fetchAllUserRolesDB :: UserId -> DB [Role]\n+fetchAllUserRolesDB user_id = fmap unwrapValues $\n     selectDistinct $\n         from $ \\pur -> do\n         where_ (pur ^. ProjectUserRoleUser ==. val user_id)\n         return (pur ^. ProjectUserRoleRole)\n \n -- | Get the current User's Roles in a Project.\n-getCurUserRoles :: ProjectId -> Handler [Role]\n-getCurUserRoles project_id = maybeAuthId >>= \\case\n+fetchCurUserRolesDB :: ProjectId -> Handler [Role]\n+fetchCurUserRolesDB project_id = maybeAuthId >>= \\case\n     Nothing -> return []\n-    Just user_id -> runDB $ getRoles user_id project_id\n+    Just user_id -> runDB $ fetchUserRolesDB user_id project_id\n \n -- | Does this User have this Role in this Project?\n-hasRole :: Role -> UserId -> ProjectId -> DB Bool\n-hasRole role user_id = fmap (elem role) . getRoles user_id\n+userHasRoleDB :: Role -> UserId -> ProjectId -> DB Bool\n+userHasRoleDB role user_id = fmap (elem role) . fetchUserRolesDB user_id\n+\n+-- | Does this User have any of these Roles in this Project?\n+userHasRolesAnyDB :: [Role] -> UserId -> ProjectId -> DB Bool\n+userHasRolesAnyDB roles user_id project_id = (or . flip map roles . flip elem) <$> fetchUserRolesDB user_id project_id\n \n -- | Get all Projects this User is affiliated with, along with each Role.\n-getProjectsAndRoles :: UserId -> DB (Map (Entity Project) (Set Role))\n-getProjectsAndRoles user_id = fmap buildMap $\n+fetchUserProjectsAndRolesDB :: UserId -> DB (Map (Entity Project) (Set Role))\n+fetchUserProjectsAndRolesDB user_id = fmap buildMap $\n     select $\n         from $ \\(p `InnerJoin` pur) -> do\n         on_ (p ^. ProjectId ==.  pur ^. ProjectUserRoleProject)\n@@ -193,57 +234,18 @@ getProjectsAndRoles user_id = fmap buildMap $\n     buildMap :: [(Entity Project, Value Role)] -> Map (Entity Project) (Set Role)\n     buildMap = foldr (\\(p, Value r) -> M.insertWith (<>) p (S.singleton r)) mempty\n \n-isProjectAdmin' :: UserId -> ProjectId -> DB Bool\n-isProjectAdmin' = hasRole Admin\n-\n-isProjectTeamMember' :: UserId -> ProjectId -> DB Bool\n-isProjectTeamMember' = hasRole TeamMember\n-\n-isProjectModerator' :: UserId -> ProjectId -> DB Bool\n-isProjectModerator' = hasRole Moderator\n-\n-isProjectAdmin :: Text -> UserId -> DB Bool\n-isProjectAdmin project_handle user_id =\n-    fmap (not . null) $ select $ from $ \\ (pur `InnerJoin` p) -> do\n-        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId\n-        where_ $ p ^. ProjectHandle ==. val project_handle\n-            &&. pur ^. ProjectUserRoleUser ==. val user_id\n-            &&. pur ^. ProjectUserRoleRole ==. val Admin\n-        limit 1\n-        return ()\n-\n-isProjectTeamMember :: Text -> UserId -> DB Bool\n-isProjectTeamMember project_handle user_id =\n-    fmap (not . null) $ select $ from $ \\ (pur `InnerJoin` p) -> do\n-        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId\n-        where_ $ p ^. ProjectHandle ==. val project_handle\n-            &&. pur ^. ProjectUserRoleUser ==. val user_id\n-            &&. pur ^. ProjectUserRoleRole ==. val TeamMember\n-        limit 1\n-        return ()\n-\n-isProjectModerator :: Text -> UserId -> DB Bool\n-isProjectModerator project_handle user_id =\n-    fmap (not . null) $ select $ from $ \\ (pur `InnerJoin` p) -> do\n-        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId\n-        where_ $ p ^. ProjectHandle ==. val project_handle\n-            &&. pur ^. ProjectUserRoleUser ==. val user_id\n-            &&. pur ^. ProjectUserRoleRole ==. val Moderator\n-        limit 1\n-        return ()\n-\n-isCurUserProjectModerator :: Text -> Handler Bool\n-isCurUserProjectModerator project_handle =\n-    maybeAuthId >>= maybe (return False) (runYDB . isProjectModerator project_handle)\n-\n-isProjectAffiliated :: Text -> UserId -> DB Bool\n-isProjectAffiliated project_handle user_id =\n-    fmap (not . null) $ select $ from $ \\ (pur `InnerJoin` p) -> do\n-        on_ $ pur ^. ProjectUserRoleProject ==. p ^. ProjectId\n-        where_ $ p ^. ProjectHandle ==. val project_handle\n-            &&. pur ^. ProjectUserRoleUser ==. val user_id\n-        limit 1\n-        return ()\n+userIsProjectAdminDB :: UserId -> ProjectId -> DB Bool\n+userIsProjectAdminDB = userHasRoleDB Admin\n+\n+userIsProjectTeamMemberDB :: UserId -> ProjectId -> DB Bool\n+userIsProjectTeamMemberDB = userHasRoleDB TeamMember\n+\n+userIsProjectModeratorDB :: UserId -> ProjectId -> DB Bool\n+userIsProjectModeratorDB = userHasRoleDB Moderator\n+\n+-- | A User is affiliated with a Project if they have *any* Role.\n+userIsAffiliatedWithProjectDB :: UserId -> ProjectId -> DB Bool\n+userIsAffiliatedWithProjectDB = userHasRolesAnyDB [minBound..maxBound]\n \n -- | Check if the current User can make the given User eligible for establishment.\n -- This is True if the current User is a Moderator of any Project, and the given User\n@@ -256,36 +258,67 @@ canMakeEligible :: UserId -> UserId -> Handler Bool\n canMakeEligible establishee_id establisher_id = do\n     (establishee, establisher_is_mod) <- runYDB $ (,)\n         <$> get404 establishee_id\n-        <*> (elem Moderator <$> getAllRoles establisher_id)\n-    return $ isUnestablished establishee && establisher_is_mod\n+        <*> (elem Moderator <$> fetchAllUserRolesDB establisher_id)\n+    return $ userIsUnestablished establishee && establisher_is_mod\n \n--- | How does this User prefer messages of a certain type to be delivered (if at all)?\n--- listToMaybe is appropriate here due to UniqueUserMessagePref (list returned will\n+-- | How does this User prefer notifications of a certain type to be delivered (if at all)?\n+-- listToMaybe is appropriate here due to UniqueUserNotificationPref (list returned will\n -- either be [] or [Value delivery])\n-fetchUserMessagePrefDB :: UserId -> MessageType -> DB (Maybe MessageDelivery)\n-fetchUserMessagePrefDB user_id msg_type = fmap (fmap unValue . listToMaybe) $\n+fetchUserNotificationPrefDB :: UserId -> NotificationType -> DB (Maybe NotificationDelivery)\n+fetchUserNotificationPrefDB user_id notif_type = fmap (fmap unValue . listToMaybe) $\n     select $\n-    from $ \\ump -> do\n+    from $ \\unp -> do\n     where_ $\n-        ump ^. UserMessagePrefUser ==. val user_id &&.\n-        ump ^. UserMessagePrefType ==. val msg_type\n-    return (ump ^. UserMessagePrefDelivery)\n+        unp ^. UserNotificationPrefUser ==. val user_id &&.\n+        unp ^. UserNotificationPrefType ==. val notif_type\n+    return (unp ^. UserNotificationPrefDelivery)\n+\n+-- | Fetch a User's unarchived private Notifications.\n+fetchUserNotificationsDB :: UserId -> DB [Entity Notification]\n+fetchUserNotificationsDB = fetchNotifs (not_ . (^. NotificationArchived))\n+\n+-- | Fetch a User's archived private Notifications.\n+fetchUserArchivedNotificationsDB :: UserId -> DB [Entity Notification]\n+fetchUserArchivedNotificationsDB = fetchNotifs (^. NotificationArchived)\n+\n+-- | Abstract fetching archived/unarchived Notifications. Unexported.\n+fetchNotifs :: (SqlExpr (Entity Notification) -> SqlExpr (Value Bool)) -> UserId -> DB [Entity Notification]\n+fetchNotifs cond user_id =\n+    select $\n+    from $ \\n -> do\n+    where_ $\n+        n ^. NotificationTo ==. val user_id &&.\n+        cond n\n+    orderBy [desc (n ^. NotificationCreatedTs)]\n+    return n\n \n userWatchProjectDB :: UserId -> ProjectId -> DB ()\n userWatchProjectDB user_id project_id = void (insertUnique (UserWatchingProject user_id project_id))\n \n userUnwatchProjectDB :: UserId -> ProjectId -> DB ()\n-userUnwatchProjectDB user_id project_id = delete_watching >> delete_views\n+userUnwatchProjectDB user_id project_id = do\n+    delete_watching\n+    delete_comment_views\n+    delete_wiki_edit_views\n   where\n     delete_watching = deleteBy (UniqueUserWatchingProject user_id project_id)\n-    delete_views =\n+\n+    delete_comment_views = delete_wiki_page_comment_views\n+\n+    delete_wiki_page_comment_views =\n         delete $\n         from $ \\vc ->\n-        where_ (vc ^. ViewCommentComment `in_` (subList_select (querProjectCommentIdsPostedOnWikiPagesDB project_id)))\n+        where_ (vc ^. ViewCommentComment `in_` subList_select (querProjectCommentsOnWikiPagesDB project_id (Just user_id)))\n+\n+    delete_wiki_edit_views =\n+        delete $\n+        from $ \\vwe ->\n+        where_ (vwe ^. ViewWikiEditEdit `in_` (subList_select (querProjectWikiEdits project_id)))\n \n-userWatchingProjectDB :: UserId -> ProjectId -> DB Bool\n-userWatchingProjectDB user_id project_id = maybe (False) (const True) <$> getBy (UniqueUserWatchingProject user_id project_id)\n+userIsWatchingProjectDB :: UserId -> ProjectId -> DB Bool\n+userIsWatchingProjectDB user_id project_id = maybe (False) (const True) <$> getBy (UniqueUserWatchingProject user_id project_id)\n \n+-- | Mark all given Comments as viewed by the given User.\n userViewCommentsDB :: UserId -> [CommentId] -> DB ()\n userViewCommentsDB user_id unfiltered_comment_ids = filteredCommentIds >>= userViewCommentsDB'\n   where\n@@ -299,3 +332,131 @@ userViewCommentsDB user_id unfiltered_comment_ids = filteredCommentIds >>= userV\n \n     userViewCommentsDB' :: [CommentId] -> DB ()\n     userViewCommentsDB' comment_ids = void (insertMany (map (ViewComment user_id) comment_ids))\n+\n+-- | Mark all given Comments as viewed by the given User, if they are watching\n+-- the given Project.\n+userMaybeViewProjectCommentsDB :: UserId -> ProjectId -> [CommentId] -> DB ()\n+userMaybeViewProjectCommentsDB user_id project_id comment_ids = do\n+    ok <- userIsWatchingProjectDB user_id project_id\n+    when ok $\n+        userViewCommentsDB user_id comment_ids\n+\n+-- | Mark all WikiEdits made on the given WikiPage as viewed by the given User.\n+userViewWikiEditsDB :: UserId -> WikiPageId -> DB ()\n+userViewWikiEditsDB user_id wiki_page_id = unviewedWikiEdits >>= viewWikiEdits\n+  where\n+    unviewedWikiEdits :: DB [WikiEditId]\n+    unviewedWikiEdits = fmap (map unValue) $\n+        select $\n+        from $ \\we -> do\n+        where_ $\n+            we ^. WikiEditPage ==. val wiki_page_id &&.\n+            we ^. WikiEditId `notIn` exprUserViewedWikiEdits user_id\n+        return (we ^. WikiEditId)\n+\n+    viewWikiEdits :: [WikiEditId] -> DB ()\n+    viewWikiEdits = mapM_ (insert_ . ViewWikiEdit user_id)\n+\n+-- | Update this User's read notifications timestamp.\n+userReadNotificationsDB :: UserId -> DB ()\n+userReadNotificationsDB user_id = liftIO getCurrentTime >>= \\now -> do\n+    update $ \\u -> do\n+    set u [UserReadNotifications =. val now]\n+    where_ (u ^. UserId ==. val user_id)\n+\n+-- | Update this User's read applications timestamp.\n+userReadVolunteerApplicationsDB :: UserId -> DB ()\n+userReadVolunteerApplicationsDB user_id = liftIO getCurrentTime >>= \\now -> do\n+    update $ \\u -> do\n+    set u [UserReadApplications =. val now]\n+    where_ (u ^. UserId ==. val user_id)\n+\n+-- | Is this User allowed to delete this Comment?\n+-- If it has any replies at all - no.\n+userCanDeleteCommentDB :: UserId -> Entity Comment -> DB Bool\n+userCanDeleteCommentDB user_id (Entity comment_id comment) = do\n+    if commentUser comment /= user_id\n+        then return False\n+        else do\n+          descendants_ids <- fetchCommentAllDescendantsDB comment_id\n+          if null descendants_ids\n+              then return True\n+              else return False\n+\n+-- | Fetch a User's number of unviewed comments on each WikiPage of a Project.\n+fetchNumUnviewedCommentsOnProjectWikiPagesDB :: UserId -> ProjectId -> DB (Map WikiPageId Int)\n+fetchNumUnviewedCommentsOnProjectWikiPagesDB user_id project_id = fmap (M.fromList . map unwrapValues) $\n+    select $\n+    from $ \\(c `InnerJoin` wp) -> do\n+    on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)\n+    where_ $\n+        exprWikiPageOnProject wp project_id &&.\n+        c ^. CommentId `notIn` exprUserViewedComments user_id\n+    groupBy (wp ^. WikiPageId)\n+    let countRows' = countRows :: SqlExpr (Value Int)\n+    having (countRows' >. val 0)\n+    return (wp ^. WikiPageId, countRows')\n+\n+fetchNumUnviewedWikiEditsOnProjectDB :: UserId -> ProjectId -> DB (Map WikiPageId Int)\n+fetchNumUnviewedWikiEditsOnProjectDB user_id project_id = fmap (M.fromList . map unwrapValues) $\n+    select $\n+    from $ \\(wp `InnerJoin` we) -> do\n+    on_ (wp ^. WikiPageId ==. we ^. WikiEditPage)\n+    where_ $\n+        exprWikiPageOnProject wp project_id &&.\n+        we ^. WikiEditId `notIn` exprUserViewedWikiEdits user_id\n+    groupBy (wp ^. WikiPageId)\n+    let countRows' = countRows :: SqlExpr (Value Int)\n+    having (countRows' >. val 0)\n+    return (wp ^. WikiPageId, countRows')\n+\n+fetchNumUnreadNotificationsDB :: UserId -> DB Int\n+fetchNumUnreadNotificationsDB user_id = fmap (\\[Value n] -> n) $\n+    select $\n+    from $ \\(u `InnerJoin` n) -> do\n+    on_ (u ^. UserId ==. n ^. NotificationTo)\n+    where_ $\n+        u ^. UserId ==. val user_id &&.\n+        n ^. NotificationCreatedTs >=. u ^. UserReadNotifications\n+    return countRows\n+\n+-- | Annotate a [CommentTag]. Returns a Map CommentId [AnnotatedTag] so this\n+-- function can be called with multiple Comments' CommentTags. If all\n+-- [CommentTag] are of the same comment, that's fine -- the returned map will\n+-- only have one key.\n+--\n+-- The [AnnotatedTag] value is left unsorted, but the [(Entity User, Int)] within each\n+-- AnnotatedTag is sorted by ascending username.\n+buildAnnotatedCommentTagsDB :: Maybe UserId -> [CommentTag] -> DB (Map CommentId [AnnotatedTag])\n+buildAnnotatedCommentTagsDB muser_id comment_tags =  do\n+    user_map <- entitiesMap <$> fetchUsersInDB (map commentTagUser comment_tags)\n+    tag_map <- entitiesMap <$> fetchTagsInDB (map commentTagTag comment_tags)\n+    -- TODO(mitchell): cached\n+    tag_colors <- maybe fetchDefaultTagColorsDB fetchTagColorsDB muser_id\n+\n+    let f :: [CommentTag] -> Map CommentId [AnnotatedTag]\n+        f = M.mapWithKey (map . i) . M.map h . g\n+\n+        -- Pair each CommentTag with its CommentId, then collect CommentTags back up,\n+        -- grouped by their CommentIds.\n+        g :: [CommentTag] -> Map CommentId [CommentTag]\n+        g = M.fromListWith (++) . map (commentTagComment &&& return)\n+\n+        -- Group each CommentTag by TagId, combining Users' votes.\n+        h :: [CommentTag] -> [(TagId, [(Entity User, Int)])]\n+        h = M.toList . foldr step mempty\n+          where\n+            step :: CommentTag -> Map TagId [(Entity User, Int)] -> Map TagId [(Entity User, Int)]\n+            step (CommentTag _ tag_id user_id n) =\n+                M.insertWith (++) tag_id [(Entity user_id (user_map M.! user_id), n)]\n+\n+        -- Construct an AnnotatedTag given all relevant info.\n+        i :: CommentId -> (TagId, [(Entity User, Int)]) -> AnnotatedTag\n+        i comment_id (tag_id, user_votes) =\n+          AnnotatedTag\n+            (Entity tag_id (tag_map M.! tag_id))\n+            (CommentTagR comment_id tag_id)\n+            (M.findWithDefault 0x77AADD tag_id tag_colors)\n+            (sortBy (compare `on` (userName . entityVal . fst)) user_votes)\n+\n+    return (f comment_tags)\ndiff --git a/Model/User/Internal.hs b/Model/User/Internal.hs\nnew file mode 100644\nindex 0000000..57e42b1\n--- /dev/null\n+++ b/Model/User/Internal.hs\n@@ -0,0 +1,18 @@\n+module Model.User.Internal where\n+\n+import Prelude\n+\n+import Model.Notification.Internal\n+\n+import Data.Text      (Text)\n+import Yesod.Markdown (Markdown)\n+\n+data UserUpdate =\n+    UserUpdate\n+        { userUpdateName               :: Maybe Text\n+        , userUpdateAvatar             :: Maybe Text\n+        , userUpdateIrcNick            :: Maybe Text\n+        , userUpdateBlurb              :: Maybe Markdown\n+        , userUpdateStatement          :: Maybe Markdown\n+--      , userUpdateNotificationPreferences :: [(NotificationType, NotificationDelivery)]\n+        }\ndiff --git a/Model/User/Sql.hs b/Model/User/Sql.hs\nindex 5eafe4e..39952af 100644\n--- a/Model/User/Sql.hs\n+++ b/Model/User/Sql.hs\n@@ -1,11 +1,13 @@\n module Model.User.Sql\n-  ( exprIsModerator\n+  ( exprUserIsModerator\n+  , exprUserViewedComments\n+  , exprUserViewedWikiEdits\n   ) where\n \n import Import\n \n-exprIsModerator :: UserId -> SqlExpr (Value ProjectId) -> SqlExpr (Value Bool)\n-exprIsModerator = exprHasRole Moderator\n+exprUserIsModerator :: UserId -> SqlExpr (Value ProjectId) -> SqlExpr (Value Bool)\n+exprUserIsModerator = exprHasRole Moderator\n \n exprHasRole :: Role -> UserId -> SqlExpr (Value ProjectId) -> SqlExpr (Value Bool)\n exprHasRole role user_id project_id =\n@@ -15,3 +17,21 @@ exprHasRole role user_id project_id =\n         r ^. ProjectUserRoleProject  ==. project_id  &&.\n         r ^. ProjectUserRoleUser     ==. val user_id &&.\n         r ^. ProjectUserRoleRole     ==. val role\n+\n+\n+-- | Expression to get all the Comments a User has viewed.\n+exprUserViewedComments :: UserId -> SqlExpr (ValueList CommentId)\n+exprUserViewedComments user_id =\n+    subList_select $\n+    from $ \\(c `InnerJoin` vc) -> do\n+    on_ (c ^. CommentId ==. vc ^. ViewCommentComment)\n+    where_ (vc ^. ViewCommentUser ==. val user_id)\n+    return (c ^. CommentId)\n+\n+-- | Expression to get all the WikiEdits a User has viewed.\n+exprUserViewedWikiEdits :: UserId -> SqlExpr (ValueList WikiEditId)\n+exprUserViewedWikiEdits user_id =\n+    subList_select $\n+    from $ \\vwe -> do\n+    where_ (vwe ^. ViewWikiEditUser ==. val user_id)\n+    return (vwe ^. ViewWikiEditEdit)\ndiff --git a/Model/Wiki.hs b/Model/Wiki.hs\nnew file mode 100644\nindex 0000000..f1b5dba\n--- /dev/null\n+++ b/Model/Wiki.hs\n@@ -0,0 +1,91 @@\n+module Model.Wiki\n+    ( createWikiEditDB\n+    , createWikiPageDB\n+    , fetchWikiPagesInDB\n+    , getAllWikiComments\n+    ) where\n+\n+import Import\n+\n+import Model.Comment.Sql\n+import Model.Discussion\n+import Model.Permission\n+import Model.Project               (getProjectPages)\n+\n+import Control.Monad.Writer.Strict (tell)\n+\n+createWikiPageDB :: Text -> ProjectId -> Markdown -> PermissionLevel -> UserId -> SDB ()\n+createWikiPageDB target project_id content permission_level user_id = do\n+    now           <- liftIO getCurrentTime\n+    discussion_id <- lift createDiscussionDB\n+    let wiki_page = WikiPage now target project_id content discussion_id permission_level\n+    wiki_page_id <- lift (insert wiki_page)\n+    -- Don't generate a WikiEdit event in addition to this WikiPage event.\n+    wiki_edit_id <- lift (insert (WikiEdit now user_id wiki_page_id content (Just "Page created.")))\n+    lift $ insert_ (WikiLastEdit wiki_page_id wiki_edit_id)\n+    tell [EWikiPage wiki_page_id wiki_page]\n+\n+createWikiEditDB :: UserId -> WikiPageId -> Markdown -> Maybe Text -> SDB WikiEditId\n+createWikiEditDB user_id wiki_page_id content mcomment = do\n+    now <- liftIO getCurrentTime\n+    let wiki_edit = WikiEdit now user_id wiki_page_id content mcomment\n+    wiki_edit_id <- lift (insert wiki_edit)\n+    tell [EWikiEdit wiki_edit_id wiki_edit]\n+    return wiki_edit_id\n+\n+fetchWikiPagesInDB :: [WikiPageId] -> DB [Entity WikiPage]\n+fetchWikiPagesInDB wiki_page_ids =\n+    select $\n+    from $ \\wp -> do\n+    where_ (wp ^. WikiPageId `in_` valList wiki_page_ids)\n+    return wp\n+\n+-- | Get the unapproved, new and old Comments on all WikiPages of Project. Takes a\n+-- UTCTime 'since' to filter comments EARLIER than this time, and a CommentId\n+-- 'latest_comment_id' to filter comments AFTER this comment (used for paging).\n+getAllWikiComments :: Maybe UserId -> ProjectId -> CommentId -> UTCTime -> Int64 -> DB ([Entity Comment], [Entity Comment], [Entity Comment])\n+getAllWikiComments mviewer_id project_id latest_comment_id since limit_num = do\n+    pages_ids           <- map entityKey <$> getProjectPages project_id\n+    unapproved_comments <- getUnapprovedComments pages_ids\n+    new_comments        <- getNewComments        pages_ids\n+    old_comments        <- getOldComments        pages_ids (limit_num - fromIntegral (length new_comments))\n+    return (unapproved_comments, new_comments, old_comments)\n+  where\n+    getUnapprovedComments :: [WikiPageId] -> DB [Entity Comment]\n+    getUnapprovedComments pages_ids =\n+        select $\n+        from $ \\(c `InnerJoin` wp) -> do\n+        on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)\n+        where_ $\n+            wp ^. WikiPageId `in_` valList pages_ids &&.\n+            exprCommentUnapproved c &&.\n+            exprCommentProjectPermissionFilter mviewer_id (val project_id) c\n+        orderBy [desc (c ^. CommentCreatedTs)]\n+        return c\n+\n+    getNewComments :: [WikiPageId] -> DB [Entity Comment]\n+    getNewComments pages_ids =\n+        select $\n+        from $ \\(c `InnerJoin` wp) -> do\n+        on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)\n+        where_ $\n+            wp ^. WikiPageId `in_` valList pages_ids &&.\n+            c ^. CommentId <=. val latest_comment_id &&.\n+            c ^. CommentApprovedTs >=. just (val since) &&.\n+            exprCommentProjectPermissionFilter mviewer_id (val project_id) c\n+        orderBy [desc (c ^. CommentApprovedTs)]\n+        limit limit_num\n+        return c\n+\n+    getOldComments :: [WikiPageId] -> Int64 -> DB [Entity Comment]\n+    getOldComments pages_ids lim =\n+        select $\n+        from $ \\(c `InnerJoin` wp) -> do\n+        on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)\n+        where_ $\n+            wp ^. WikiPageId `in_` valList pages_ids &&.\n+            c ^. CommentApprovedTs <. just (val since) &&.\n+            exprCommentProjectPermissionFilter mviewer_id (val project_id) c\n+        orderBy [desc (c ^. CommentApprovedTs)]\n+        limit lim\n+        return c\ndiff --git a/Model/Wiki/Sql.hs b/Model/Wiki/Sql.hs\nnew file mode 100644\nindex 0000000..fbd361e\n--- /dev/null\n+++ b/Model/Wiki/Sql.hs\n@@ -0,0 +1,13 @@\n+module Model.Wiki.Sql where\n+\n+import Import\n+\n+exprEditViewedBy :: UserId -> SqlExpr (Entity WikiEdit) -> SqlExpr (Value Bool)\n+exprEditViewedBy user_id we = we ^. WikiEditId `in_`\n+    (subList_select $\n+     from $ \\vwe -> do\n+     where_ (vwe ^. ViewWikiEditUser ==. val user_id)\n+     return (vwe ^. ViewWikiEditEdit))\n+\n+exprWikiPageOnProject :: SqlExpr (Entity WikiPage) -> ProjectId -> SqlExpr (Value Bool)\n+exprWikiPageOnProject wp project_id = wp ^. WikiPageProject ==. val project_id\ndiff --git a/Model/WikiPage.hs b/Model/WikiPage.hs\ndeleted file mode 100644\nindex 8347c67..0000000\n--- a/Model/WikiPage.hs\n+++ /dev/null\n@@ -1,66 +0,0 @@\n-module Model.WikiPage\n-    ( getAllWikiComments\n-    , fetchWikiPagesInDB\n-    ) where\n-\n-import Import\n-\n-import Model.Comment.Sql\n-import Model.Project     (getProjectPages)\n-\n-fetchWikiPagesInDB :: [WikiPageId] -> DB [Entity WikiPage]\n-fetchWikiPagesInDB wiki_page_ids =\n-    select $\n-    from $ \\wp -> do\n-    where_ (wp ^. WikiPageId `in_` valList wiki_page_ids)\n-    return wp\n-\n--- | Get the unapproved, new and old Comments on all WikiPages of Project. Takes a\n--- UTCTime 'since' to filter comments EARLIER than this time, and a CommentId\n--- 'latest_comment_id' to filter comments AFTER this comment (used for paging).\n-getAllWikiComments :: Maybe UserId -> ProjectId -> CommentId -> UTCTime -> Int64 -> DB ([Entity Comment], [Entity Comment], [Entity Comment])\n-getAllWikiComments mviewer_id project_id latest_comment_id since limit_num = do\n-    pages_ids           <- map entityKey <$> getProjectPages project_id\n-    unapproved_comments <- getUnapprovedComments pages_ids\n-    new_comments        <- getNewComments        pages_ids\n-    old_comments        <- getOldComments        pages_ids (limit_num - fromIntegral (length new_comments))\n-    return (unapproved_comments, new_comments, old_comments)\n-  where\n-    getUnapprovedComments :: [WikiPageId] -> DB [Entity Comment]\n-    getUnapprovedComments pages_ids =\n-        select $\n-        from $ \\(c `InnerJoin` wp) -> do\n-        on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)\n-        where_ $\n-            wp ^. WikiPageId `in_` valList pages_ids &&.\n-            exprUnapproved c &&.\n-            exprPermissionFilter mviewer_id (val project_id) c\n-        orderBy [desc (c ^. CommentCreatedTs)]\n-        return c\n-\n-    getNewComments :: [WikiPageId] -> DB [Entity Comment]\n-    getNewComments pages_ids =\n-        select $\n-        from $ \\(c `InnerJoin` wp) -> do\n-        on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)\n-        where_ $\n-            wp ^. WikiPageId `in_` valList pages_ids &&.\n-            c ^. CommentId <=. val latest_comment_id &&.\n-            c ^. CommentModeratedTs >=. just (val since) &&.\n-            exprPermissionFilter mviewer_id (val project_id) c\n-        orderBy [desc (c ^. CommentModeratedTs)]\n-        limit limit_num\n-        return c\n-\n-    getOldComments :: [WikiPageId] -> Int64 -> DB [Entity Comment]\n-    getOldComments pages_ids lim =\n-        select $\n-        from $ \\(c `InnerJoin` wp) -> do\n-        on_ (c ^. CommentDiscussion ==. wp ^. WikiPageDiscussion)\n-        where_ $\n-            wp ^. WikiPageId `in_` valList pages_ids &&.\n-            c ^. CommentModeratedTs <. just (val since) &&.\n-            exprPermissionFilter mviewer_id (val project_id) c\n-        orderBy [desc (c ^. CommentModeratedTs)]\n-        limit lim\n-        return c\ndiff --git a/Model/WikiPage/Sql.hs b/Model/WikiPage/Sql.hs\ndeleted file mode 100644\nindex 510b9c0..0000000\n--- a/Model/WikiPage/Sql.hs\n+++ /dev/null\n@@ -1,13 +0,0 @@\n-module Model.WikiPage.Sql where\n-\n-import Import\n-\n-exprEditViewedBy :: UserId -> SqlExpr (Entity WikiEdit) -> SqlExpr (Value Bool)\n-exprEditViewedBy user_id we = we ^. WikiEditId `in_`\n-    (subList_select $\n-     from $ \\vwe -> do\n-     where_ (vwe ^. ViewWikiEditUser ==. val user_id)\n-     return (vwe ^. ViewWikiEditEdit))\n-\n-exprWikiPageOnProject :: SqlExpr (Entity WikiPage) -> ProjectId -> SqlExpr (Value Bool)\n-exprWikiPageOnProject wp project_id = wp ^. WikiPageProject ==. val project_id\ndiff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 82e3026..2afbd6c 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -28,16 +28,19 @@ library\n                      Foundation\n                      Import\n                      Model\n-                     Model.AnnotatedTag\n+                     Model.Application\n                      Model.CollapseState\n                      Model.Comment\n+                     Model.Comment.ActionPermissions\n+                     Model.Comment.HandlerInfo\n+                     Model.Comment.Routes\n                      Model.Comment.Sql\n                      Model.Currency\n                      Model.Discussion\n                      Model.Issue\n                      Model.Markdown\n                      Model.Markdown.Diff\n-                     Model.Message\n+                     Model.Notification\n                      Model.Permission\n                      Model.Project\n                      Model.Project.Sql\n@@ -47,37 +50,34 @@ library\n                      Model.SnowdriftEvent\n                      Model.SnowdriftEvent.Internal\n                      Model.Tag\n-                     Model.Ticket\n                      Model.Transaction\n                      Model.User\n+                     Model.User.Internal\n                      Model.User.Sql\n                      Model.ViewType\n-                     Model.WikiPage\n-                     Model.WikiPage.Sql\n+                     Model.Wiki\n+                     Model.Wiki.Sql\n                      Settings\n                      Settings.StaticFiles\n                      Settings.Development\n-                     Handler.Application\n-                     Handler.Applications\n+                     SnowdriftEventHandler\n                      Handler.BuildFeed\n-                     Handler.Contact\n+                     Handler.Comment\n+                     Handler.Discussion\n                      Handler.Home\n                      Handler.HonorPledge\n                      Handler.Invitation\n-                     Handler.Invite\n                      Handler.JsLicense\n                      Handler.MarkdownTutorial\n-                     Handler.Messages\n+                     Handler.Notification\n                      Handler.PostLogin\n                      Handler.Privacy\n                      Handler.Project\n                      Handler.RepoFeed\n-                     Handler.Tickets\n                      Handler.ToU\n                      Handler.UpdateShares\n                      Handler.User\n-                     Handler.UserBalance\n-                     Handler.UserPledges\n+                     Handler.Utils\n                      Handler.Volunteer\n                      Handler.Who\n                      Handler.Widget\n@@ -88,6 +88,7 @@ library\n                      View.User\n                      View.Wiki\n                      View.PledgeButton\n+                     View.Project\n                      Version\n                      Widgets.Doc\n                      Widgets.Markdown\n@@ -100,7 +101,7 @@ library\n     other-modules: Model.CollapseState.Internal\n                    Model.Comment.Internal\n                    Model.Established.Internal\n-                   Model.Message.Internal\n+                   Model.Notification.Internal\n                    Model.Permission.Internal\n                    Model.Role.Internal\n                    Model.Settings.Internal\n@@ -108,7 +109,7 @@ library\n \n     if flag(dev) || flag(library-only)\n         cpp-options:   -DDEVELOPMENT\n-        ghc-options:   -Wall -O0\n+        ghc-options:   -Wall -O0 -fhpc\n     else\n         ghc-options:   -Werror -Wall -O2\n \n@@ -257,6 +258,8 @@ test-suite test\n     hs-source-dirs:    tests\n     ghc-options:       -Wall\n \n+    extensions: QuasiQuotes\n+\n     build-depends: base\n                  , Snowdrift\n                  , yesod-test\n@@ -281,3 +284,7 @@ test-suite test\n                  , xml-conduit\n                  , esqueleto\n                  , HUnit\n+                 , template-haskell\n+                 , haskell-src-exts\n+                 , haskell-src-meta\n+                 , lifted-base\ndiff --git a/SnowdriftEventHandler.hs b/SnowdriftEventHandler.hs\nnew file mode 100644\nindex 0000000..c910d13\n--- /dev/null\n+++ b/SnowdriftEventHandler.hs\n@@ -0,0 +1,99 @@\n+module SnowdriftEventHandler\n+    ( snowdriftEventHandlers\n+    ) where\n+\n+import Import\n+\n+import           Model.Discussion\n+import           Model.Notification\n+import           Model.Project\n+import           Model.User\n+\n+import           Blaze.ByteString.Builder (toLazyByteString)\n+import           Control.Monad.Reader\n+import           Data.Maybe               (fromJust)\n+import qualified Data.Text.Lazy           as TL\n+import qualified Data.Text.Lazy.Encoding  as TLE\n+import qualified Database.Persist\n+import           Yesod                    (renderRoute)\n+import           Yesod.Markdown\n+\n+-- Add more event handlers here.\n+snowdriftEventHandlers :: [SnowdriftEvent -> Daemon ()]\n+snowdriftEventHandlers =\n+    [ notificationEventHandler\n+    , eventInserterHandler\n+    ]\n+\n+-- | Handler in charge of sending Notifications to interested parties.\n+notificationEventHandler :: SnowdriftEvent -> Daemon ()\n+-- Notify the comment's parent's poster that their comment has been replied to (per their preferences).\n+notificationEventHandler (ECommentPosted comment_id comment) = case commentParent comment of\n+    Nothing -> return ()\n+    Just parent_comment_id -> do\n+        (parent_user_id, delivery) <- runDB $ do\n+            parent_user_id <- commentUser <$> Database.Persist.getJust parent_comment_id\n+            delivery <- fetchUserNotificationPrefDB parent_user_id NotifReply\n+            return (parent_user_id, delivery)\n+        -- Any non-Nothing delivery implies an internal Notification should be sent.\n+        when (isJust delivery) $ do\n+            app <- ask\n+            let parent_comment_route = renderRoute' (CommentDirectLinkR parent_comment_id) app\n+                reply_comment_route  = renderRoute' (CommentDirectLinkR comment_id)        app\n+\n+            let content = mconcat\n+                  [ "Someone replied to [your comment]("\n+                  , Markdown parent_comment_route\n+                  , ")! You can view the reply [here]("\n+                  , Markdown reply_comment_route\n+                  , ")."\n+                  , ""\n+                  , "*You can filter these notifications by adjusting the settings in your profile.*"\n+                  ]\n+            runSDB (sendNotificationDB_ NotifReply parent_user_id Nothing content)\n+-- Notify all moderators of the project the comment was posted on.\n+notificationEventHandler (ECommentPending comment_id comment) = do\n+    app <- ask\n+    runSDB $ lift (fetchDiscussionProjectDB (commentDiscussion comment)) >>= \\case\n+        Nothing -> return () -- Comment wasn't on a project, somehow? I guess do nothing.\n+        Just project_id -> do\n+            project <- getJust project_id\n+\n+            let content = mconcat\n+                  [ "An unapproved comment has been posted on a "\n+                  , Markdown (projectName project)\n+                  , " page. Please view it [here]("\n+                  , Markdown (renderRoute' (CommentDirectLinkR comment_id) app)\n+                  , ")."\n+                  ]\n+\n+            lift (fetchProjectModeratorsDB project_id) >>=\n+                -- Send the notification, and record the fact that we send it (so we can\n+                -- later delete it, when the comment is approved).\n+                mapM_ (\\user_id -> sendNotificationDB NotifUnapprovedComment user_id Nothing content\n+                                     >>= insert_ . UnapprovedCommentNotification comment_id)\n+notificationEventHandler (ENotificationSent _ _)       = return ()\n+notificationEventHandler (EWikiEdit _ _)          = return ()\n+notificationEventHandler (EWikiPage _ _)          = return ()\n+notificationEventHandler (ENewPledge _ _)         = return ()\n+notificationEventHandler (EUpdatedPledge _ _ _)   = return ()\n+notificationEventHandler (EDeletedPledge _ _ _ _) = return ()\n+\n+-- | Handler in charge of inserting events (stripped down) into a separate table for each type.\n+eventInserterHandler :: SnowdriftEvent -> Daemon ()\n+-- If an unapproved comment is sent as an ECommentPosted event, bad things will happen (fromJust).\n+eventInserterHandler (ECommentPosted comment_id Comment{..})                         = runDB (insert_ (EventCommentPosted (fromJust commentApprovedTs) comment_id))\n+eventInserterHandler (ECommentPending comment_id Comment{..})                        = runDB (insert_ (EventCommentPending commentCreatedTs comment_id))\n+eventInserterHandler (ENotificationSent notif_id Notification{..})                   = runDB (insert_ (EventNotificationSent notificationCreatedTs notif_id))\n+eventInserterHandler (EWikiPage wiki_page_id WikiPage{..})                           = runDB (insert_ (EventWikiPage wikiPageCreatedTs wiki_page_id))\n+eventInserterHandler (EWikiEdit wiki_edit_id WikiEdit{..})                           = runDB (insert_ (EventWikiEdit wikiEditTs wiki_edit_id))\n+eventInserterHandler (ENewPledge shares_pledged_id SharesPledged{..})                = runDB (insert_ (EventNewPledge sharesPledgedTs shares_pledged_id))\n+eventInserterHandler (EUpdatedPledge old_shares shares_pledged_id SharesPledged{..}) = runDB (insert_ (EventUpdatedPledge sharesPledgedTs old_shares shares_pledged_id))\n+eventInserterHandler (EDeletedPledge ts user_id project_id shares)                   = runDB (insert_ (EventDeletedPledge ts user_id project_id shares))\n+\n+renderRoute' :: Route App -> App -> Text\n+renderRoute' route app =\n+    let (path_pieces, query_params) = renderRoute route\n+    -- converting a lazy ByteString to a strict Text... ridiculous!\n+    -- why does joinPath return a ByteString??\n+    in TL.toStrict $ TLE.decodeUtf8 $ toLazyByteString (joinPath app "" path_pieces query_params)\ndiff --git a/View/Comment.hs b/View/Comment.hs\nindex 07eee56..ad63679 100644\n--- a/View/Comment.hs\n+++ b/View/Comment.hs\n@@ -1,38 +1,45 @@\n module View.Comment\n-    ( commentCloseForm\n-    , commentCloseFormWidget\n-    , commentEditForm\n-    , commentEditFormWidget\n+    ( approveCommentFormWidget\n+    , closeCommentForm\n+    , closeCommentFormWidget\n     , commentForm\n     , commentFormWidget\n     , commentNewTopicForm\n+    , commentNewTopicFormWidget\n     , commentReplyForm\n-    , commentRetractForm\n-    , commentRetractFormWidget\n+    , commentReplyFormWidget\n     , commentTreeWidget\n+    , commentWidget\n     , createCommentTagForm\n+    , deleteCommentFormWidget\n     , disabledCommentForm\n+    , editCommentForm\n+    , editCommentFormWidget\n     , flagCommentForm\n+    , flagCommentFormWidget\n     , newCommentTagForm\n     , orderingNewestFirst\n-    , rethreadForm\n+    , rethreadCommentForm\n+    , rethreadCommentFormWidget\n+    , retractCommentForm\n+    , retractCommentFormWidget\n     ) where\n \n import Import\n \n-import Model.AnnotatedTag\n import Model.Comment\n-import Model.Markdown\n-import Model.Tag               (TagMap)\n+import Model.Comment.ActionPermissions\n+import Model.Comment.Routes\n+import Model.Tag\n import Model.User\n+import View.User\n import Widgets.Markdown\n import Widgets.Tag\n import Widgets.Time\n \n--- import           Control.Lens  ((%~), _3)\n-import qualified Data.List                 as L\n-import qualified Data.Map                  as M\n-import qualified Data.Text                 as T\n+import qualified Data.List as L\n+import qualified Data.Map  as M\n+import qualified Data.Text as T\n import           Data.Tree\n \n disabledCommentForm :: Form Markdown\n@@ -51,38 +58,54 @@ commentFormWidget' form = do\n         <div>\n             <form method="POST" enctype=#{enctype}>\n                 ^{widget}\n-                <input type="submit" name="mode" value="preview">\n+                <button type="submit" name="mode" value="preview">preview\n     |]\n \n-commentEditForm :: Markdown -> Form Markdown\n-commentEditForm = commentForm "Edit" . Just\n+closeCommentForm    :: Maybe Markdown -> Form Markdown\n+commentNewTopicForm ::                   Form Markdown\n+commentReplyForm    ::                   Form Markdown\n+editCommentForm     :: Markdown       -> Form Markdown\n+retractCommentForm  :: Maybe Markdown -> Form Markdown\n \n-commentEditFormWidget :: Markdown -> Widget\n-commentEditFormWidget = commentFormWidget' . commentEditForm\n-\n-commentNewTopicForm :: Form Markdown\n+closeCommentForm    = commentForm "Reason for closing:"\n commentNewTopicForm = commentForm "New Topic" Nothing\n+commentReplyForm    = commentForm "Reply"     Nothing\n+editCommentForm     = commentForm "Edit"      . Just\n+retractCommentForm  = commentForm "Reason for retracting:"\n+\n+closeCommentFormWidget    :: Maybe Markdown -> Widget\n+commentNewTopicFormWidget ::                   Widget\n+commentReplyFormWidget    ::                   Widget\n+editCommentFormWidget     :: Markdown       -> Widget\n+retractCommentFormWidget  :: Maybe Markdown -> Widget\n+\n+closeCommentFormWidget    = commentFormWidget' . closeCommentForm\n+commentNewTopicFormWidget = commentFormWidget' commentNewTopicForm\n+commentReplyFormWidget    = commentFormWidget' commentReplyForm\n+editCommentFormWidget     = commentFormWidget' . editCommentForm\n+retractCommentFormWidget  = commentFormWidget' . retractCommentForm\n+\n+approveCommentFormWidget :: Widget\n+approveCommentFormWidget =\n+    [whamlet|\n+        <form method="POST">\n+            <button type="submit" name="mode" value="post">approve post\n+    |]\n \n-commentReplyForm :: Form Markdown\n-commentReplyForm = commentForm "Reply" Nothing\n-\n-commentCloseForm :: Maybe Markdown -> Form Markdown\n-commentCloseForm = commentForm "Reason for closing:"\n-\n-commentCloseFormWidget :: Maybe Markdown -> Widget\n-commentCloseFormWidget = commentFormWidget' . commentCloseForm\n-\n-commentRetractForm :: Maybe Markdown -> Form Markdown\n-commentRetractForm = commentForm "Reason for retracting:"\n-\n-commentRetractFormWidget :: Maybe Markdown -> Widget\n-commentRetractFormWidget = commentFormWidget' . commentRetractForm\n-\n-rethreadForm :: Form (Text, Text)\n-rethreadForm = renderBootstrap3 $ (,)\n+rethreadCommentForm :: Form (Text, Text)\n+rethreadCommentForm = renderBootstrap3 $ (,)\n     <$> areq' textField "New Parent Url" Nothing\n     <*> areq' textField "Reason" Nothing\n \n+rethreadCommentFormWidget :: Widget\n+rethreadCommentFormWidget = do\n+    (form, enctype) <- handlerToWidget (generateFormPost rethreadCommentForm)\n+    [whamlet|\n+        <form method=post enctype=#{enctype}>\n+            ^{form}\n+            <button type="submit" name="mode" value="post">rethread\n+    |]\n+\n createCommentTagForm :: Form Text\n createCommentTagForm = renderBootstrap3 $ areq' textField "" Nothing\n \n@@ -107,203 +130,164 @@ flagCommentForm def_reasons def_message = renderBootstrap3 $ (,) <$> flagReasons\n     additionalCommentsForm :: AForm Handler (Maybe Markdown)\n     additionalCommentsForm = aopt' snowdriftMarkdownField "Optional: add helpful comments to clarify the issue and/or suggestions for improvement" def_message\n \n+flagCommentFormWidget :: Maybe (Maybe [FlagReason]) -> Maybe (Maybe Markdown) -> Widget\n+flagCommentFormWidget def_reasons def_message = do\n+    (form, enctype) <- handlerToWidget (generateFormPost (flagCommentForm def_reasons def_message))\n+    [whamlet|\n+        <form method="POST" enctype=#{enctype}>\n+            <h4>Code of Conduct Violation(s):\n+            ^{form}\n+            <button type="submit" name="mode" value="preview">preview flag message\n+    |]\n+\n+deleteCommentFormWidget :: Widget\n+deleteCommentFormWidget =\n+    [whamlet|\n+        <div>\n+            <form method=POST>\n+                <button type="submit" name="mode" value="post">delete\n+                <button type="submit" name="mode" value="cancel">cancel\n+    |]\n+\n+-- | Order comment trees by newest-first, taking the root and all children of each\n+-- tree into consideration (essentially compares each tree's newest comment,\n+-- no matter how deeply nested).\n+orderingNewestFirst :: Tree (Entity Comment) -> Tree (Entity Comment) -> Ordering\n+orderingNewestFirst = flip (compare `on` (timestamp . newest))\n+  where\n+    newest :: Tree (Entity Comment) -> Entity Comment\n+    newest = L.maximumBy (compare `on` timestamp) . flatten\n+\n+    timestamp :: Entity Comment -> UTCTime\n+    timestamp = commentCreatedTs . entityVal\n+\n+expandCommentWidget :: Int -> MaxDepth -> Widget\n+expandCommentWidget num_replies new_max_depth = do\n+    Just cur_route <- getCurrentRoute\n+    let new_route = case new_max_depth of\n+                        NoMaxDepth -> (cur_route, [])\n+                        MaxDepth n -> (cur_route, [("maxdepth", T.pack (show n))])\n+    [whamlet|\n+        <br>\n+        <br>\n+        <em>\n+            <a href="@?{new_route}">\n+                #{num_replies} more #{plural num_replies "reply" "replies"}\n+    |]\n+\n -- | An entire comment tree.\n-commentTreeWidget :: Widget                -- ^ Form to display under the root comment.\n-                  -> Tree (Entity Comment) -- ^ Comment tree.\n-                  -> [CommentClosure]      -- ^ Earlier closures.\n+commentTreeWidget :: Widget                       -- ^ Form to display under the root comment.\n+                  -> Tree (Entity Comment)        -- ^ Comment tree.\n+                  -> Maybe UserId                 -- ^ Viewer.\n+                  -> CommentRoutes                -- ^ Comment routes.\n+                  -> MakeCommentActionPermissions -- ^ Action permissions maker.\n+                  -> [CommentClosure]             -- ^ Earlier closures.\n                   -> UserMap\n                   -> ClosureMap\n                   -> TicketMap\n                   -> FlagMap\n-                  -> TagMap\n-                  -> Text                  -- ^ Project handle.\n-                  -> Text                  -- ^ Wiki page name.\n-                  -> Bool                  -- ^ Show actions? (false, for preview)\n-                  -> Int                   -- ^ Max depth.\n-                  -> Int                   -- ^ Depth.\n+                  -> Bool                         -- ^ Is preview?\n+                  -> MaxDepth                     -- ^ Max depth.\n+                  -> Int                          -- ^ Depth.\n                   -> Widget\n commentTreeWidget form_under_root_comment\n                   (Node root_entity@(Entity root_id root) children)\n+                  mviewer_id\n+                  comment_routes\n+                  make_action_permissions\n                   earlier_closures\n                   user_map\n                   closure_map\n                   ticket_map\n                   flag_map\n-                  tag_map\n-                  project_handle\n-                  target\n-                  show_actions\n+                  is_preview\n                   max_depth\n                   depth = do\n-    let inner_widget =\n+    let num_children = length children\n+        inner_widget =\n             form_under_root_comment <>\n-            expandCommentOrChildrenWidget\n-                children\n-                user_map\n-                closure_map\n-                ticket_map\n-                flag_map\n-                tag_map\n-                project_handle\n-                target\n-                show_actions\n-                max_depth\n-                (depth+1)\n-\n+            if MaxDepth depth > max_depth && num_children > 0\n+                then expandCommentWidget num_children (addMaxDepth max_depth 2) -- FIXME(mitchell): arbitrary '2' here\n+                else forM_ children $ \\child ->\n+                         commentTreeWidget\n+                           mempty\n+                           child\n+                           mviewer_id\n+                           comment_routes\n+                           make_action_permissions\n+                           [] -- don't want to show earlier closures on *all* comments, just the first one.\n+                           user_map\n+                           closure_map\n+                           ticket_map\n+                           flag_map\n+                           is_preview\n+                           max_depth\n+                           (depth+1)\n+\n+    action_permissions <- handlerToWidget (make_action_permissions root_entity)\n     commentWidget\n         root_entity\n+        mviewer_id\n+        comment_routes\n+        action_permissions\n         earlier_closures\n-        (user_map M.! commentUser root)\n+        (fromMaybe (error "comment user missing from user map") (M.lookup (commentUser root) user_map))\n         (M.lookup root_id closure_map)\n         (M.lookup root_id ticket_map)\n         (M.lookup root_id flag_map)\n-        tag_map\n-        project_handle\n-        target\n-        show_actions\n+        is_preview\n         inner_widget\n \n-expandCommentOrChildrenWidget :: [Tree (Entity Comment)] -- ^ Children comments.\n-                              -> UserMap\n-                              -> ClosureMap\n-                              -> TicketMap\n-                              -> FlagMap\n-                              -> TagMap\n-                              -> Text                    -- ^ Project handle.\n-                              -> Text                    -- ^ Wiki page name.\n-                              -> Bool                    -- ^ Show actions? (false, for preview)\n-                              -> Int                     -- ^ Max depth.\n-                              -> Int                     -- ^ Depth.\n-                              -> Widget\n-expandCommentOrChildrenWidget children\n-                              user_map\n-                              closure_map\n-                              ticket_map\n-                              flag_map\n-                              tag_map\n-                              project_handle\n-                              target\n-                              show_actions\n-                              max_depth\n-                              depth = do\n-    let num_children = length children\n-    if depth > max_depth && num_children > 0\n-        then expandCommentWidget num_children (max_depth + 2) -- FIXME(mitchell): arbitrary '2' here\n-        else forM_ children $ \\child ->\n-                 commentTreeWidget\n-                     mempty\n-                     child\n-                     [] -- don't want to show earlier closures on *all* comments, just the first one.\n-                     user_map\n-                     closure_map\n-                     ticket_map\n-                     flag_map\n-                     tag_map\n-                     project_handle\n-                     target\n-                     show_actions\n-                     max_depth\n-                     depth\n-\n -- | A "single" comment, which also displays an 'inner widget' inside of it.\n -- The reason this can't be made more modular is the HTML for nested comments\n -- requires us to render the entire tree (can't close the parent comment's div\n -- before the children comments).\n commentWidget :: Entity Comment                       -- ^ Comment.\n+              -> Maybe UserId                         -- ^ Viewer.\n+              -> CommentRoutes                        -- ^ Comment routes.\n+              -> CommentActionPermissions             -- ^ Permissions for comment actions.\n               -> [CommentClosure]                     -- ^ Earlier closures.\n               -> User                                 -- ^ Comment poster.\n               -> Maybe CommentClosure                 -- ^ Is this closed?\n               -> Maybe (Entity Ticket)                -- ^ Is this a ticket?\n               -> Maybe (Maybe Markdown, [FlagReason]) -- ^ Is this comment flagged?\n-              -> TagMap                               -- ^ Tag map.\n-              -> Text                                 -- ^ Project handle.\n-              -> Text                                 -- ^ Wiki page name.\n-              -> Bool                                 -- ^ Show actions?\n+              -> Bool                                 -- ^ Is this a preview?\n               -> Widget                               -- ^ Inner widget (children comments, 'expand' link, reply box, etc)\n               -> Widget\n-commentWidget c@(Entity comment_id comment)\n+commentWidget (Entity comment_id comment)\n+              mviewer_id\n+              CommentRoutes{..}\n+              CommentActionPermissions{..}\n               earlier_closures\n               user\n               mclosure\n               mticket\n               mflag\n-              tag_map\n-              project_handle\n-              target\n-              show_actions\n+              is_preview\n               inner_widget = do\n     let user_id       = commentUser comment\n-        is_unapproved = not . isApproved $ comment\n-        is_top_level  = isTopLevel  comment\n-        is_even_depth = isEvenDepth comment\n-        is_odd_depth  = isOddDepth  comment\n-\n-    (is_mod, can_establish, can_reply, can_retract, can_close, can_edit, can_delete, can_rethread, can_add_tag, can_flag) <-\n-        handlerToWidget $ makeViewerPermissions (Entity user_id user) project_handle target c\n-\n-    tags <- fmap (L.sortBy (compare `on` atName)) . handlerToWidget $ do\n-        runDB (getCommentTags comment_id) >>=\n-          annotateCommentTags tag_map project_handle target comment_id . map entityVal\n+        is_unapproved = not . commentIsApproved $ comment\n+        is_top_level  = commentIsTopLevel  comment\n+        is_even_depth = commentIsEvenDepth comment\n+        is_odd_depth  = commentIsOddDepth  comment\n+\n+    -- TODO(mitchell): Lots of refactoring to lift this database hit up to the\n+    -- controller layer. This currently has horrible performance - a hit *per* comment!\n+    tags <- handlerToWidget $ runDB $\n+        maybe [] sortAnnotTagsByName .\n+          M.lookup comment_id <$>\n+            (fetchCommentCommentTagsDB comment_id >>= buildAnnotatedCommentTagsDB mviewer_id)\n+\n+    let ticket_str = case mticket of\n+            Just (Entity (Key (PersistInt64 tid)) _) -> T.pack $ show tid\n+            _ -> "???"\n+\n+-- error about ambiguity about monad type in this, needs to be adjusted to\n+-- fit the idea that comments aren't necessarily on wiki pages\n+--      prettyTicketLine line =\n+--          let pretty title = "<div class='ticket-title'>SD-" <> ticket_str <> ": " <> title <> "</div>"\n+--           in return $ maybe line pretty $ T.stripPrefix "ticket: " line\n+\n+--      commentTextTransform = prettyTicketLine\n \n     $(widgetFile "comment")\n-\n-makeViewerPermissions :: Entity User    -- comment poster\n-                      -> Text\n-                      -> Text\n-                      -> Entity Comment\n-                      -> Handler ( Bool -- is moderator?\n-                                 , Bool -- can establish?\n-                                 , Bool -- can reply?\n-                                 , Bool -- can retract?\n-                                 , Bool -- can close?\n-                                 , Bool -- can edit?\n-                                 , Bool -- can delete?\n-                                 , Bool -- can rethread?\n-                                 , Bool -- can add tag?\n-                                 , Bool -- can flag?\n-                                 )\n-makeViewerPermissions (Entity poster_id poster) project_handle target comment_entity@(Entity comment_id comment) = do\n-    Just current_route <- getCurrentRoute\n-    let is_reply_route = current_route == ReplyCommentR project_handle target comment_id\n-\n-    maybeAuth >>= \\case\n-        Nothing -> return (False, False, not is_reply_route, False, False, False, False, False, False, False)\n-        Just (Entity viewer_id viewer) -> do\n-            (is_mod, can_delete, is_flagged) <- runDB $ (,,)\n-                <$> isProjectModerator project_handle viewer_id\n-                <*> canDeleteComment viewer_id comment_entity\n-                <*> isFlagged comment_id\n-\n-            let can_establish = is_mod && estIsUnestablished (userEstablished poster)\n-                can_reply     = not is_reply_route && not is_flagged\n-                can_retract   = poster_id == viewer_id\n-                can_close     = isEstablished viewer\n-                can_edit      = canEditComment viewer_id comment\n-                can_rethread  = poster_id == viewer_id || is_mod\n-                can_add_tag   = isEstablished viewer\n-                can_flag      = isEstablished viewer && viewer_id /= poster_id && not is_flagged\n-\n-            return (is_mod, can_establish, can_reply, can_retract, can_close, can_edit,\n-                    can_delete, can_rethread, can_add_tag, can_flag)\n-\n--- Order comment trees by newest-first, taking the root and all children of each\n--- tree into consideration.\n-orderingNewestFirst :: Tree (Entity Comment) -> Tree (Entity Comment) -> Ordering\n-orderingNewestFirst = flip (compare `on` (timestamp . newest))\n-  where\n-    newest :: Tree (Entity Comment) -> Entity Comment\n-    newest = L.maximumBy (compare `on` timestamp) . flatten\n-\n-    timestamp :: Entity Comment -> UTCTime\n-    timestamp = commentCreatedTs . entityVal\n-\n-expandCommentWidget :: Int -> Int -> Widget\n-expandCommentWidget num_replies new_max_depth = do\n-    Just cur_route <- getCurrentRoute\n-    [whamlet|\n-        <br>\n-        <br>\n-        <em>\n-            <a href="@?{(cur_route, [("maxdepth", T.pack (show new_max_depth))])}">\n-                #{num_replies} more #{plural num_replies "reply" "replies"}\n-    |]\ndiff --git a/View/Discussion.hs b/View/Discussion.hs\nnew file mode 100644\nindex 0000000..a56ead1\n--- /dev/null\n+++ b/View/Discussion.hs\n@@ -0,0 +1,11 @@\n+module View.Discussion where\n+\n+import Model.Comment.Internal\n+\n+discussionCommentTree :: CommentMods              -- ^ Comment structure modifications.\n+                      -> CommentActionPermissions\n+                      -> MaxDepth\n+                      -> Bool           -- ^ Is preview?\n+                      -> Widget         -- ^ Widget to display under root comment.\n+                      -> CommentId      -- ^ Root comment id.\n+                      -> Handler (Widget, Tree (Entity Comment))\ndiff --git a/View/Project.hs b/View/Project.hs\nnew file mode 100644\nindex 0000000..bd7fde5\n--- /dev/null\n+++ b/View/Project.hs\n@@ -0,0 +1,114 @@\n+module View.Project\n+    ( editProjectForm\n+    , inviteForm\n+    , projectBlogForm\n+    , renderBlogPost\n+    , renderProject\n+    , viewForm\n+    ) where\n+\n+\n+import Import\n+\n+import           Data.Filter\n+import           Data.Order\n+import           Model.Currency\n+import           Model.Markdown\n+import           Model.Project\n+import           Model.Shares\n+import           Model.Role\n+import           Widgets.Markdown\n+\n+import qualified Data.Text       as T\n+import           Data.Time.Clock\n+import           Yesod.Markdown\n+\n+renderProject :: Maybe ProjectId -> Project -> [Int64] -> Maybe (Entity Pledge) -> WidgetT App IO ()\n+renderProject maybe_project_id project pledges pledge = do\n+    let share_value = projectShareValue project\n+        users = fromIntegral $ length pledges\n+        shares = sum pledges\n+        project_value = share_value $* fromIntegral shares\n+        description = markdownWidget (projectHandle project) $ projectDescription project\n+\n+        maybe_shares = pledgeShares . entityVal <$> pledge\n+\n+    now <- liftIO getCurrentTime\n+\n+    amounts <- case projectLastPayday project of\n+        Nothing -> return Nothing\n+        Just last_payday -> handlerToWidget $ runDB $ do\n+            -- This assumes there were transactions associated with the last payday\n+            [Value (Just last) :: Value (Maybe Rational)] <-\n+                select $\n+                from $ \\ transaction -> do\n+                where_ $\n+                    transaction ^. TransactionPayday ==. val (Just last_payday) &&.\n+                    transaction ^. TransactionCredit ==. val (Just $ projectAccount project)\n+                return $ sum_ $ transaction ^. TransactionAmount\n+\n+            [Value (Just year) :: Value (Maybe Rational)] <-\n+                select $\n+                from $ \\ (transaction `InnerJoin` payday) -> do\n+                where_ $\n+                    payday ^. PaydayDate >. val (addUTCTime (-365 * 24 * 60 * 60) now) &&.\n+                    transaction ^. TransactionCredit ==. val (Just $ projectAccount project)\n+                on_ $ transaction ^. TransactionPayday ==. just (payday ^. PaydayId)\n+                return $ sum_ $ transaction ^. TransactionAmount\n+\n+            [Value (Just total) :: Value (Maybe Rational)] <-\n+                select $\n+                from $ \\ transaction -> do\n+                where_ $ transaction ^. TransactionCredit ==. val (Just $ projectAccount project)\n+                return $ sum_ $ transaction ^. TransactionAmount\n+\n+            return $ Just (Milray $ round last, Milray $ round year, Milray $ round total)\n+\n+\n+    ((_, update_shares), _) <- handlerToWidget $ generateFormGet $ maybe previewPledgeForm pledgeForm maybe_project_id\n+\n+    $(widgetFile "project")\n+\n+renderBlogPost :: Text -> ProjectBlog -> WidgetT App IO ()\n+renderBlogPost project_handle blog_post = do\n+    let (Markdown top_content) = projectBlogTopContent blog_post\n+        (Markdown bottom_content) = maybe (Markdown "") ("***\\n" <>) $ projectBlogBottomContent blog_post\n+        title = projectBlogTitle blog_post\n+        content = markdownWidget project_handle $ Markdown $ T.snoc top_content '\\n' <> bottom_content\n+\n+    $(widgetFile "blog_post")\n+\n+editProjectForm :: Maybe (Project, [Text]) -> Form UpdateProject\n+editProjectForm project =\n+    renderBootstrap3 $ UpdateProject\n+        <$> areq' textField "Project Name" (projectName . fst <$> project)\n+        <*> areq' snowdriftMarkdownField "Description" (projectDescription . fst <$> project)\n+        <*> (maybe [] (map T.strip . T.splitOn ",") <$> aopt' textField "Tags" (Just . T.intercalate ", " . snd <$> project))\n+        <*> aopt' textField "Github Repository" (projectGithubRepo . fst <$> project)\n+\n+projectBlogForm :: Maybe (Text, Text, Markdown) -> Form (UTCTime -> UserId -> ProjectId -> DiscussionId -> ProjectBlog)\n+projectBlogForm defaults = renderBootstrap3 $\n+    let getTitle (title, _, _) = title\n+        getHandle (_, handle, _) = handle\n+        getContent (_, _, content) = content\n+     in mkBlog\n+        <$> areq' textField "Post Title" (getTitle <$> defaults)\n+        <*> areq' textField "Post Handle" (getHandle <$> defaults)\n+        <*> areq' snowdriftMarkdownField "Content" (getContent <$> defaults)\n+  where\n+    mkBlog :: Text -> Text -> Markdown -> (UTCTime -> UserId -> ProjectId -> DiscussionId -> ProjectBlog)\n+    mkBlog title handle (Markdown content) now user_id project_id discussion_id =\n+        let (top_content, bottom_content) = break (== "***") $ T.lines content\n+         in ProjectBlog now title handle user_id project_id\n+                discussion_id (Markdown $ T.unlines top_content)\n+                (if null bottom_content then Nothing else Just $ Markdown $ T.unlines bottom_content)\n+\n+inviteForm :: Form (Text, Role)\n+inviteForm = renderBootstrap3 $ (,)\n+    <$> areq' textField "About this invitation:" Nothing\n+    <*> areq roleField "Type of Invite:" (Just TeamMember)\n+\n+viewForm :: Form (Filterable -> Bool, Orderable -> [Double])\n+viewForm = renderBootstrap3 $ (,)\n+    <$> (either (const defaultFilter) id . parseFilterExpression . fromMaybe "" <$> aopt' textField "filter" Nothing)\n+    <*> (either (const defaultOrder) id . parseOrderExpression . fromMaybe "" <$> aopt' textField "sort" Nothing)\ndiff --git a/View/SnowdriftEvent.hs b/View/SnowdriftEvent.hs\nindex f138389..7cb3047 100644\n--- a/View/SnowdriftEvent.hs\n+++ b/View/SnowdriftEvent.hs\n@@ -4,23 +4,221 @@ module View.SnowdriftEvent where\n \n import Import\n \n-renderCommentPostedOnWikiPageEvent :: CommentId -> Comment -> Entity WikiPage -> Widget\n-renderCommentPostedOnWikiPageEvent comment_id comment (Entity _ wiki_page) =\n+import Model.Comment\n+import Model.Comment.ActionPermissions\n+import Model.Comment.Routes\n+import Model.User\n+import View.Comment\n+import Widgets.Time\n+\n+import qualified Data.Map as M\n+\n+renderProjectCommentPostedEvent\n+        :: CommentId\n+        -> Comment\n+        -> Project\n+        -> Maybe UserId\n+        -> Map CommentId [CommentClosure]\n+        -> UserMap\n+        -> ClosureMap\n+        -> TicketMap\n+        -> FlagMap\n+        -> Widget\n+renderProjectCommentPostedEvent\n+        comment_id\n+        comment\n+        Project{..}\n+        mviewer_id\n+        earlier_closures_map\n+        user_map\n+        closure_map\n+        ticket_map\n+        flag_map = do\n+    let comment_entity = Entity comment_id comment\n+        comment_routes = projectCommentRoutes projectHandle\n+\n+    action_permissions <- handlerToWidget (makeProjectCommentActionPermissions projectHandle comment_entity)\n+\n+    let comment_widget =\n+            commentWidget\n+              comment_entity\n+              mviewer_id\n+              comment_routes\n+              action_permissions\n+              (M.findWithDefault [] comment_id earlier_closures_map)\n+              (fromMaybe (error "renderProjectCommentPostedEvent: comment user missing from user map")\n+                         (M.lookup (commentUser comment) user_map))\n+              (M.lookup comment_id closure_map)\n+              (M.lookup comment_id ticket_map)\n+              (M.lookup comment_id flag_map)\n+              False\n+              mempty\n+\n     [whamlet|\n-        <div>On #{wikiPageTarget wiki_page}: #{commentText comment}\n-            \\ <a href=@{CommentDirectLinkR comment_id}>(permalink)\n+        <div .event>\n+            On\n+            <a href=@{ProjectR projectHandle}>#{projectName}#\n+            :\n+\n+            ^{comment_widget}\n+    |]\n+\n+renderWikiPageCommentPostedEvent\n+        :: CommentId\n+        -> Comment\n+        -> Entity WikiPage\n+        -> Text\n+        -> Maybe UserId\n+        -> Map CommentId [CommentClosure]\n+        -> UserMap\n+        -> ClosureMap\n+        -> TicketMap\n+        -> FlagMap\n+        -> Widget\n+renderWikiPageCommentPostedEvent\n+        comment_id\n+        comment\n+        (Entity _ wiki_page)\n+        project_handle\n+        mviewer_id\n+        earlier_closures_map\n+        user_map\n+        closure_map\n+        ticket_map\n+        flag_map = do\n+    let comment_entity = Entity comment_id comment\n+        target = wikiPageTarget wiki_page\n+        comment_routes = wikiPageCommentRoutes project_handle target\n+\n+    action_permissions <- handlerToWidget (makeProjectCommentActionPermissions project_handle comment_entity)\n+\n+    let comment_widget =\n+            commentWidget\n+              comment_entity\n+              mviewer_id\n+              comment_routes\n+              action_permissions\n+              (M.findWithDefault [] comment_id earlier_closures_map)\n+              (fromMaybe (error "renderWikiPageCommentPostedEvent: comment user missing from user map")\n+                         (M.lookup (commentUser comment) user_map))\n+              (M.lookup comment_id closure_map)\n+              (M.lookup comment_id ticket_map)\n+              (M.lookup comment_id flag_map)\n+              False\n+              mempty\n+\n+\n+    [whamlet|\n+        <div .event>\n+            On the\n+            <a href=@{WikiR project_handle target}>#{target}\n+            wiki page:\n+\n+            ^{comment_widget}\n     |]\n \n -- This should really *never* be called, but it's included in case of nuclear meltdown.\n renderCommentPostedOnUnknownDiscussionEvent :: CommentId -> Comment -> Widget\n renderCommentPostedOnUnknownDiscussionEvent comment_id comment =\n     [whamlet|\n-        <div>#{commentText comment}\n-            \\ <a href=@{CommentDirectLinkR comment_id}>(permalink)\n+        <div .event>\n+            $maybe approved_ts <- commentApprovedTs comment\n+                ^{renderTime approved_ts}\n+            $nothing\n+                ^{renderTime $ commentCreatedTs comment}\n+            somehow, this\n+            <a href=@{CommentDirectLinkR comment_id}> comment\n+            was posted on an unknown discussion: #\n+            #{commentText comment}\n+    |]\n+\n+renderCommentPendingEvent :: CommentId -> Comment -> UserMap -> Widget\n+renderCommentPendingEvent comment_id comment user_map = do\n+    let poster = fromMaybe (error "renderCommentPendingEvent: poster not found in user map")\n+                           (M.lookup (commentUser comment) user_map)\n+    [whamlet|\n+        <div .event>\n+            ^{renderTime $ commentCreatedTs comment}\n+            <a href=@{UserR (commentUser comment)}> #{userDisplayName (Entity (commentUser comment) poster)}\n+            posted a\n+            <a href=@{CommentDirectLinkR comment_id}> comment\n+            awaiting moderator approval:\n+            #{commentText comment}\n+    |]\n+\n+renderWikiPageEvent :: Text -> WikiPageId -> WikiPage -> UserMap -> Widget\n+renderWikiPageEvent project_handle _ wiki_page user_map = do\n+--\n+-- The commented stuff here (and in the whamlet commented part)\n+-- is because there's no wikiPageUser yet and the\n+-- user_map is also not needed until this is active--\n+--    let editor = fromMaybe\n+--            (error "renderWikiPageEvent: wiki editor not found in user map")\n+--            (M.lookup (wikiPageUser wiki_page) user_map)\n+--\n+    [whamlet|\n+        <div .event>\n+            ^{renderTime $ wikiPageCreatedTs wiki_page}\n+            <!--\n+                <a href=@{UserR (wikiPageUser wiki_page)}>\n+                    #{userDisplayName (Entity (wikiPageUser wiki_page) editor)}\n+                -->\n+            made a new wiki page: #\n+            <a href=@{WikiR project_handle (wikiPageTarget wiki_page)}>#{wikiPageTarget wiki_page}\n+    |]\n+\n+renderWikiEditEvent :: Text -> WikiEditId -> WikiEdit -> Map WikiPageId WikiPage -> UserMap -> Widget\n+renderWikiEditEvent project_handle edit_id wiki_edit wiki_page_map user_map = do\n+    let editor = fromMaybe (error "renderWikiEditEvent: wiki editor not found in user map")\n+                           (M.lookup (wikiEditUser wiki_edit) user_map)\n+        wiki_page = fromMaybe (error "renderWikiEditEvent: wiki page not found in wiki page map")\n+                              (M.lookup (wikiEditPage wiki_edit) wiki_page_map)\n+    [whamlet|\n+        <div .event>\n+            ^{renderTime $ wikiEditTs wiki_edit}\n+            <a href=@{UserR (wikiEditUser wiki_edit)}>\n+                #{userDisplayName (Entity (wikiEditUser wiki_edit) editor)}\n+            edited the\n+            <a href=@{WikiR project_handle (wikiPageTarget wiki_page)}> #{wikiPageTarget wiki_page}\n+            wiki page: #\n+            $maybe comment <- wikiEditComment wiki_edit\n+                #{comment}\n+            <a style="float:right" href="@{WikiEditR project_handle (wikiPageTarget wiki_page) edit_id}">\n+                see this edit version <!-- TODO: make this link to the diff instead -->\n+    |]\n+\n+renderNewPledgeEvent :: SharesPledgedId -> SharesPledged -> UserMap -> Widget\n+renderNewPledgeEvent _ SharesPledged{..} user_map = do\n+    let pledger = fromMaybe (error "renderNewPledgeEvent: pledger not found in user map")\n+                            (M.lookup sharesPledgedUser user_map)\n+    [whamlet|\n+        <div .event>\n+            ^{renderTime sharesPledgedTs}\n+            <a href=@{UserR sharesPledgedUser}> #{userDisplayName (Entity sharesPledgedUser pledger)}\n+            pledged #{show sharesPledgedShares} new shares!\n+    |]\n+\n+renderUpdatedPledgeEvent :: Int64 -> SharesPledgedId -> SharesPledged -> UserMap -> Widget\n+renderUpdatedPledgeEvent old_shares _ SharesPledged{..} user_map = do\n+    let pledger = fromMaybe (error "renderUpdatedPledgeEvent: pledger not found in user map")\n+                            (M.lookup sharesPledgedUser user_map)\n+        (verb, punc) = if old_shares < sharesPledgedShares\n+                           then ("increased", "!")\n+                           else ("decreased", ".") :: (Text, Text)\n+    [whamlet|\n+        <div .event>\n+            ^{renderTime sharesPledgedTs}\n+            <a href=@{UserR sharesPledgedUser}> #{userDisplayName (Entity sharesPledgedUser pledger)}\n+            #{verb} their pledge from #{show old_shares} to #{show sharesPledgedShares} shares#{punc}\n     |]\n \n-renderWikiEditEvent :: WikiEditId -> WikiEdit -> Entity WikiPage -> Widget\n-renderWikiEditEvent _ _ (Entity _ wiki_page) =\n+renderDeletedPledgeEvent :: UTCTime -> UserId -> Int64 -> UserMap -> Widget\n+renderDeletedPledgeEvent ts user_id shares user_map = do\n+    let pledger = fromMaybe (error "renderDeletedPledgeEvent: pledger not found in user map")\n+                            (M.lookup user_id user_map)\n     [whamlet|\n-        <div>#{wikiPageTarget wiki_page} edit!\n+        <div .event>\n+            ^{renderTime ts}\n+            <a href=@{UserR user_id}>#{userDisplayName (Entity user_id pledger)}\n+            withdrew their #{show shares}-share pledge.\n     |]\ndiff --git a/View/User.hs b/View/User.hs\nindex 95f7020..c528f5f 100644\n--- a/View/User.hs\n+++ b/View/User.hs\n@@ -1,16 +1,20 @@\n module View.User\n-    ( createUserForm\n+    ( addTestCashForm\n+    , createUserForm\n     , editUserForm\n     , establishUserForm\n     , previewUserForm\n     , renderUser\n+    , userNameWidget\n     ) where\n \n import Import\n \n+import           Model.Currency\n import           Model.Markdown\n import           Model.Role\n import           Model.User\n+import           Model.User.Internal\n import           Widgets.Markdown       (snowdriftMarkdownField)\n import           Widgets.ProjectPledges\n \n@@ -83,7 +87,7 @@ editUserForm muser = renderBootstrap3 $\n         <*> aopt' textField               "IRC nick @freenode.net)"                        (userIrcNick                   <$> muser)\n         <*> aopt' snowdriftMarkdownField  "Blurb (used on listings of many people)"        (userBlurb                     <$> muser)\n         <*> aopt' snowdriftMarkdownField  "Personal Statement (visible only on this page)" (userStatement                 <$> muser)\n-        -- <*> aopt' messagePreferencesField "Message filter"                                 (Just . userMessagePreferences <$> muser)\n+--      <*> error "TODO: notification preferences"\n \n -- | Form to mark a user as eligible for establishment. The user is fully established\n -- when s/he accepts the honor pledge.\n@@ -98,10 +102,7 @@ previewUserForm User{..} = renderBootstrap3 $\n         <*> aopt hiddenField "" (Just userIrcNick)\n         <*> hiddenMarkdown userBlurb\n         <*> hiddenMarkdown userStatement\n-        -- <*> aopt messagePreferencesField "" (Just (Just userMessagePreferences))\n-\n--- messagePreferencesField :: Field Handler [MessagePreference]\n--- messagePreferencesField = checkboxesFieldList (map (showMessagePreference &&& id) [minBound..maxBound])\n+--      <*> error "TODO: notification preferences"\n \n -- | Render a User profile, including\n renderUser :: Maybe UserId -> UserId -> User -> Map (Entity Project) (Set Role) -> Widget\n@@ -120,3 +121,17 @@ renderUser mviewer_id user_id user projects_and_roles = do\n hiddenMarkdown :: Maybe Markdown -> AForm Handler (Maybe Markdown)\n hiddenMarkdown Nothing               = fmap (fmap Markdown) $ aopt hiddenField "" Nothing\n hiddenMarkdown (Just (Markdown str)) = fmap (fmap Markdown) $ aopt hiddenField "" (Just $ Just str)\n+\n+userNameWidget :: UserId -> Widget\n+userNameWidget user_id = do\n+    maybe_user <- handlerToWidget $ runDB $ get user_id\n+    case maybe_user of\n+        Nothing -> [whamlet|deleted user|]\n+        Just user ->\n+            [whamlet|\n+                <a href=@{UserR user_id}>\n+                    #{userDisplayName (Entity user_id user)}\n+            |]\n+\n+addTestCashForm :: Form Milray\n+addTestCashForm = renderBootstrap3 $ fromInteger . (10000 *) <$> areq' intField "Add (fake) money to your account (in whole dollars)" (Just 10)\ndiff --git a/View/Wiki.hs b/View/Wiki.hs\nindex c021961..a63b5d5 100644\n--- a/View/Wiki.hs\n+++ b/View/Wiki.hs\n@@ -18,5 +18,5 @@ editWikiPermissionsForm level = renderBootstrap3 $ areq permissionLevelField "Pe\n newWikiForm :: Maybe Markdown -> Form Markdown\n newWikiForm content = renderBootstrap3 $ areq' snowdriftMarkdownField "Page Content" content\n \n-renderWiki :: Int -> Text -> Text -> Bool -> Bool -> WikiPage -> Widget\n-renderWiki comment_count project_handle target can_edit can_view_meta page = $(widgetFile "wiki")\n+renderWiki :: Int -> Text -> Text -> Bool -> WikiPage -> Widget\n+renderWiki comment_count project_handle target can_edit page = $(widgetFile "wiki")\ndiff --git a/Widgets/Navbar.hs b/Widgets/Navbar.hs\nindex ed49aa9..b40f83d 100644\n--- a/Widgets/Navbar.hs\n+++ b/Widgets/Navbar.hs\n@@ -12,46 +12,22 @@ navbar = do\n \n     alreadyExpired\n \n-    money_info <- case maybe_user of\n-        Nothing -> return Nothing\n+    (money_info, num_unread_notifs) <- case maybe_user of\n+        Nothing -> return (Nothing, 0)\n         Just (Entity user_id user) -> do\n-            (pledges, balance) <- handlerToWidget $ runDB $ do\n+            (pledges, balance, num_unread_notifs) <- handlerToWidget $ runDB $ do\n                 pledges :: [(Entity Project, Entity Pledge)] <- select $ from $\n                     \\ (project `InnerJoin` pledge) -> do\n                         on_ $ pledge ^. PledgeProject ==. project ^. ProjectId\n                         where_ $ pledge ^. PledgeUser ==. val user_id\n                         return (project, pledge)\n-\n                 Just account <- get (userAccount user)\n-                return (pledges, accountBalance account)\n+                num_unread_notifs <- fetchNumUnreadNotificationsDB user_id\n+                return (pledges, accountBalance account, num_unread_notifs)\n \n             let pledged = sum $ map (\\ (project, pledge) ->\n                     ((projectShareValue (entityVal project) $*) . fromIntegral . pledgeFundedShares . entityVal) pledge) pledges\n \n-            return $ Just (balance, pledged)\n-\n-\n-    -- TODO: make stuff below generalize to project affiliates instead of snowdrift only\n-\n-    messages <- case maybe_user of\n-        Nothing -> return []\n-        Just (Entity user_id user) -> handlerToWidget $ runDB $ do\n-            snowdrift_member <- isProjectAffiliated "snowdrift" user_id\n-\n-            select $\n-             from $ \\message -> do\n-             where_ $\n-                let\n-                    readSnowdriftMessages =\n-                        ( message ^. MessageCreatedTs >=. val (userReadMessages user)\n-                            &&. isNothing (message ^. MessageTo))\n-\n-                    readUserMessages =\n-                        ( message ^. MessageCreatedTs >=. val (userReadMessages user)\n-                            &&. message ^. MessageTo ==. val (Just user_id))\n-                 in if snowdrift_member\n-                        then readSnowdriftMessages ||. readUserMessages\n-                        else readUserMessages\n-             return message\n+            return $ (Just (balance, pledged), num_unread_notifs)\n \n     $(widgetFile "navbar")\ndiff --git a/Widgets/Preview.hs b/Widgets/Preview.hs\nindex ce80738..9caa1e4 100644\n--- a/Widgets/Preview.hs\n+++ b/Widgets/Preview.hs\n@@ -5,20 +5,21 @@ import Import\n previewWidget :: Widget -> Text -> Widget -> Widget\n previewWidget form action widget =\n     [whamlet|\n-        <form method="POST">\n-            <div .alert .alert-danger>\n-                This is a preview; changes have <em>not</em> been saved!\n-                Edit and/or submit your posting <a href="#edit-preview">below</a>.\n+        <div .alert .alert-danger>\n+            This is a preview; changes have <em>not</em> been saved!\n+            Edit and/or submit your posting <a href="#edit-preview">below</a>.\n \n-            ^{widget}\n+        ^{widget}\n \n-            <input type=submit name=mode value="preview">\n-            <input .preview-action-button type=submit name=mode value="#{action}">\n \n-            <hr>\n+        <form #edit-preview method="POST">\n+            <div .alert .alert-danger>\n+                This is a preview; your changes have <em>not</em> been saved!\n+            <button type="submit" name="mode" value="preview">preview\n+            <button .preview-action-button type="submit" name="mode" value="post">#{action}\n \n             ^{form}\n \n-            <input type=submit name=mode value="preview">\n-            <input .preview-action-button type=submit name=mode value="#{action}">\n+            <button type="submit" name="mode" value="preview">preview\n+            <button .preview-action-button type="submit" name="mode" value="post">#{action}\n     |]\ndiff --git a/Widgets/Tag.hs b/Widgets/Tag.hs\nindex ca24756..8b91610 100644\n--- a/Widgets/Tag.hs\n+++ b/Widgets/Tag.hs\n@@ -1,4 +1,3 @@\n-\n module Widgets.Tag where\n \n import Import\n@@ -6,7 +5,7 @@ import Import\n import Data.List (maximumBy)\n import Data.Bits\n \n-import Model.AnnotatedTag\n+import Model.Tag\n import Text.Printf\n \n import Model.Settings\n@@ -30,26 +29,26 @@ tagWidget t = do\n \n     show_tag_voting <- userSettingsShowTagVotes <$> handlerToWidget getUserSettings\n \n-    let my_tag = any ((== maybe_user_id) . Just . entityKey . fst) $ atUserVotes t\n+    let my_tag = any ((== maybe_user_id) . Just . entityKey . fst) $ annotTagUserVotes t\n \n-    let maybe_user_score = maybe_user_id >>= atUserScore t\n+    let maybe_user_score = maybe_user_id >>= annotTagUserScore t\n \n     let bg :: String\n-        bg = printf "%06x" $ (\\ (Color c) -> c) $ atColor t\n+        bg = printf "%06x" $ (\\ (Color c) -> c) $ annotTagColor t\n         fg :: String\n-        fg = printf "%06x" $ pickForegroundColor $ (\\ (Color c) -> c) $ atColor t\n+        fg = printf "%06x" $ pickForegroundColor $ (\\ (Color c) -> c) $ annotTagColor t\n \n     toWidget [hamlet|\n-        <form .tag action=@{atUrl t} style="background-color:##{bg};color:##{fg}" method=post>\n+        <form .tag action=@{annotTagUrl t} style="background-color:##{bg};color:##{fg}" method=post>\n             <small>\n-                #{atName t}\n+                #{annotTagName t}\n                 $if show_tag_voting\n                     <input type=submit name=direction style="color:##{fg}" value=- .tag-input>\n                     <span .tag-score>\n                         $maybe user_score <- maybe_user_score\n-                            #{user_score}/#{atScoreString t}\n+                            #{user_score}/#{annotTagScoreString t}\n                         $nothing\n-                            #{atScoreString t}\n+                            #{annotTagScoreString t}\n                     <input type=submit name=direction style="color:##{fg}" value=+ .tag-input>\n                 $else\n                     $if my_tag\ndiff --git a/comment.cassius b/comment.cassius\nnew file mode 100644\nindex 0000000..22ba502\n--- /dev/null\n+++ b/comment.cassius\n@@ -0,0 +1,78 @@\n+.comment\n+    padding : 0 .8em 0.3em 1em\n+    margin-top : 1.8em\n+    border-bottom-left-radius: 1em\n+    font-size : 15px\n+\n+.comment p, .comment ul, .comment ol, .comment h1, .comment h2, .comment h3, .comment h4, .comment h5, .comment h6\n+    margin : .5em 0\n+\n+.comment figure\n+    text-align : left\n+\n+.comment h1\n+    font-size : large\n+\n+.comment h2\n+    font-size : medium\n+\n+.comment blockquote\n+    font-size : 14px\n+    margin : 1em\n+\n+.ticket-title\n+    background-color: lightblue;\n+        border-radius: 1em;\n+            padding: 0.5em;\n+\n+.comment-head, .comment-action\n+    margin-right : 1em\n+    margin-bottom : 0.5em\n+    display : inline-block\n+    padding : 2px 5px\n+\n+.top_level\n+    border-left : solid black 0.2em\n+\n+.even_depth\n+    border-left : solid lightblue 0.2em\n+\n+.odd_depth\n+    border-left : solid lightgrey 0.2em\n+\n+.small_avatar\n+    width : 2.5em\n+    height : 2.5em\n+    padding : 0em\n+    margin-right : 0.3em\n+    border-radius : 5px\n+\n+.closed\n+    color : goldenrod\n+    border-left : solid goldenrod 0.25em\n+    padding-left : 0.5em\n+\n+.retracted\n+    color : darkred\n+    border-left : solid darkred 0.25em\n+    padding-left : 0.5em\n+\n+.flagged\n+    border : thin solid darkred\n+    border-left : solid red\n+    padding : .5em\n+    margin : .5em 0\n+    display : table\n+\n+.flag-reasons\n+    color : darkred\n+    max-width : 41em\n+\n+.flag-reasons, .flag-markdown\n+    background : whitesmoke\n+    padding : .5em\n+    margin : .5em 0\n+    display : table\n+\n+.preview a\n+    pointer-events: none\ndiff --git a/config/models b/config/models\nindex 4436634..489feed 100644\n--- a/config/models\n+++ b/config/models\n@@ -29,25 +29,26 @@ User\n     blurb Markdown Maybe\n     statement Markdown Maybe\n     ircNick Text Maybe\n-    readMessages UTCTime default=now()\n+    readNotifications UTCTime default=now() -- The last time the user visited /notifications\n     readApplications UTCTime default=now()\n     established Established default='EstUnestablished'\n+    discussion DiscussionId default=nextval('discussion_id_seq'::regclass)\n     UniqueUser ident\n     UniqueUserAccount account\n     deriving Show Typeable\n \n--- A single message preference. If some MessageType does not appear\n--- in any row for a particular user, that means the user does not\n--- wish to be notified of those messages.\n+-- A single notification preference. If some NotificationType does not appear in any\n+-- row for a particular user, that means the user does not wish to be notified\n+-- of those notifications.\n --\n--- Messages that "must" be delivered (e.g. administrative messages\n--- directly from Snowdrift) don't need an entry in this table, as\n--- the code that sends such messages needn't query it.\n-UserMessagePref\n+-- Notifications that "must" be delivered (e.g. administrative notifications\n+-- directly from Snowdrift) don't need an entry in this table, as the code that\n+-- sends such notifications needn't query it.\n+UserNotificationPref\n     user UserId\n-    type MessageType\n-    delivery MessageDelivery\n-    UniqueUserMessagePref user type\n+    type NotificationType\n+    delivery NotificationDelivery\n+    UniqueUserNotificationPref user type\n \n -- User-watching-project relation\n UserWatchingProject\n@@ -91,6 +92,7 @@ Project\n     shareValue Milray\n     lastPayday PaydayId Maybe\n     githubRepo Text Maybe\n+    discussion DiscussionId default=nextval('discussion_id_seq'::regclass)\n     UniqueProjectAccount account\n     UniqueProjectHandle handle\n     deriving Eq Show\n@@ -98,11 +100,13 @@ Project\n ProjectBlog\n     time UTCTime\n     title Text\n+    handle Text\n     user UserId\n     project ProjectId\n     discussion DiscussionId\n     topContent Markdown\n     bottomContent Markdown Maybe\n+    UniqueProjectBlogPost project handle\n     deriving Show\n \n ProjectUserRole\n@@ -123,7 +127,11 @@ ProjectLastUpdate\n     update ProjectUpdateId\n     UniqueProjectLastUpdate project\n \n+-- Parts of Pledge are duplicated in EventDeletedPledge.\n+-- If you modify Pledge, be sure to (possibly) modify\n+-- EventDeletedPledge as well!\n Pledge\n+    createdTs UTCTime default=now()\n     user UserId\n     project ProjectId\n     shares Int64\n@@ -161,18 +169,24 @@ VolunteerInterest\n     volunteer VolunteerApplicationId\n     interest InterestId\n \n-Message\n-    type MessageType default='MessageDirect'\n-    project ProjectId Maybe\n+Notification\n     createdTs UTCTime\n-    from UserId Maybe\n-    to UserId Maybe\n+    type NotificationType\n+    to UserId\n+    project ProjectId Maybe -- An "associated" project.\n     content Markdown\n-    -- Whether the message was sent automatically by Snowdrift, or by an actual user.\n-    automated Bool default=false\n-    deriving Eq\n+    archived Bool           -- Put into the "trash bin".\n+\n+-- A many-to-one relationship linking notifications sent to moderators about an\n+-- unapproved comment. When the comment is approved, all such notifications are\n+-- deleted automatically (as they are no longer relevant) - this table\n+-- allows us to keep track of which notifications to delete.\n+UnapprovedCommentNotification\n+    comment CommentId\n+    notification NotificationId\n \n WikiPage\n+    createdTs UTCTime default=now()\n     target Text\n     project ProjectId\n     content Markdown\n@@ -201,10 +215,10 @@ Comment\n     createdTs UTCTime\n \n     -- "Unestablished" users may still make comments, but they must be approved\n-    -- by a moderator. "Established" users' comments are marked as moderated by\n+    -- by a moderator. "Established" users' comments are marked as approved by\n     -- themselves (though this does not mean they are a moderator).\n-    moderatedTs UTCTime Maybe\n-    moderatedBy UserId Maybe\n+    approvedTs UTCTime Maybe\n+    approvedBy UserId Maybe\n \n     discussion DiscussionId\n     parent CommentId Maybe\n@@ -241,12 +255,6 @@ CommentFlagging\n     ts UTCTime\n     flagger UserId\n     comment CommentId\n-\n-    -- These two fields are required to reconstruct the permalink to the comment,\n-    -- for inclusion in messages to the flagger/flaggee.\n-    projectHandle Text\n-    target Text\n-\n     message Markdown Maybe -- Optional message provided by the flagger.\n     -- Only one flagging can exist for a comment at any given time.\n     UniqueCommentFlagging comment\n@@ -340,6 +348,7 @@ PledgeFormRendered\n SharesPledged\n     ts UTCTime\n     user UserId\n+    project ProjectId\n     shares Int64\n     render PledgeFormRenderedId\n \n@@ -349,18 +358,40 @@ SharesPledged\n \n -- An approved comment.\n EventCommentPosted\n-    comment CommentId\n     ts UTCTime\n+    comment CommentId\n \n -- An unapproved comment.\n EventCommentPending\n+    ts UTCTime\n     comment CommentId\n+\n+-- Notification sent event.\n+EventNotificationSent\n     ts UTCTime\n+    notification NotificationId\n \n-EventMessageSent\n-    message MessageId\n+-- Wiki page created event.\n+EventWikiPage\n     ts UTCTime\n+    wikiPage WikiPageId\n \n+-- Wiki edit made event.\n EventWikiEdit\n+    ts UTCTime\n     wikiEdit WikiEditId\n+\n+EventNewPledge\n+    ts UTCTime\n+    sharesPledged SharesPledgedId\n+\n+EventUpdatedPledge\n     ts UTCTime\n+    oldShares Int64\n+    sharesPledged SharesPledgedId\n+\n+EventDeletedPledge\n+    ts UTCTime\n+    user UserId\n+    project ProjectId\n+    shares Int64\ndiff --git a/config/routes b/config/routes\nindex 1f4d4db..0227525 100644\n--- a/config/routes\n+++ b/config/routes\n@@ -11,7 +11,6 @@\n /dest               PostLoginR        GET\n /honor-pledge       HonorPledgeR      GET POST\n /license/javascript JsLicenseR        GET\n-/messages           MessagesR         GET\n /priv               PrivacyR          GET\n /tou                ToUR              GET\n /tutorial/markdown  MarkdownTutorialR GET\n@@ -19,73 +18,91 @@\n -- User\n \n /u                 UsersR           GET\n+/u/!new            UserCreateR      GET POST\n /u/#UserId         UserR            GET POST\n+/u/#UserId/balance UserBalanceR     GET POST\n+/u/#UserId/d       UserDiscussionR  GET POST\n /u/#UserId/edit    EditUserR        GET\n /u/#UserId/elig    UserEstEligibleR     POST\n-/u/#UserId/balance UserBalanceR     GET POST\n /u/#UserId/pledges UserPledgesR     GET\n-/u/!new            UserCreateR      GET POST\n+\n+-- Notifications\n+\n+/notifications             NotificationsR         GET\n+/notifications/archived    ArchivedNotificationsR GET\n+/n/#NotificationId/archive ArchiveNotificationR       POST\n \n -- Project\n \n-/p                                           ProjectsR            GET\n-/p/#Text                                     ProjectR             GET POST\n-/p/#Text/applications                        ApplicationsR        GET\n-/p/#Text/application/#VolunteerApplicationId ApplicationR         GET\n-/p/#Text/b                                   ProjectBlogR         GET POST\n-/p/#Text/b/#ProjectBlogId                    ProjectBlogPostR     GET\n-/p/#Text/contact                             ContactR             GET POST\n-/p/#Text/edit                                EditProjectR         GET\n-/p/#Text/feed                                ProjectFeedR         GET\n-/p/#Text/invitation/#Text                    InvitationR          GET POST\n-/p/#Text/invite                              InviteR              GET POST\n-/p/#Text/patrons                             ProjectPatronsR      GET\n-/p/#Text/button.png                          ProjectPledgeButtonR GET\n-/p/#Text/shares                              UpdateSharesR        GET POST\n-/p/#Text/t                                   TicketsR             GET\n-/p/#Text/transactions                        ProjectTransactionsR GET\n-/p/#Text/volunteer                           VolunteerR           GET POST\n-/p/#Text/who                                 WhoR                 GET\n-/p/#Text/widget                              WidgetR              GET\n+/p                                           ProjectsR                GET\n+/p/#Text                                     ProjectR                 GET POST\n+/p/#Text/applications                        ApplicationsR            GET\n+/p/#Text/application/#VolunteerApplicationId ApplicationR             GET\n+/p/#Text/blog                                ProjectBlogR             GET\n+/p/#Text/blog/!new                           NewProjectBlogPostR      GET POST\n+/p/#Text/blog/#Text                          ProjectBlogPostR         GET\n+/p/#Text/c/#CommentId                        ProjectCommentR          GET\n+/p/#Text/c/#CommentId/close                  CloseProjectCommentR     GET POST\n+/p/#Text/c/#CommentId/delete                 DeleteProjectCommentR    GET POST\n+/p/#Text/c/#CommentId/edit                   EditProjectCommentR      GET POST\n+/p/#Text/c/#CommentId/flag                   FlagProjectCommentR      GET POST\n+/p/#Text/c/#CommentId/moderate               ApproveProjectCommentR   GET POST\n+/p/#Text/c/#CommentId/reply                  ReplyProjectCommentR     GET POST\n+/p/#Text/c/#CommentId/rethread               RethreadProjectCommentR  GET POST\n+/p/#Text/c/#CommentId/retract                RetractProjectCommentR   GET POST\n+/p/#Text/c/#CommentId/tag/!new               ProjectCommentAddTagR    GET\n+/p/#Text/c/#CommentId/tags                   ProjectCommentTagsR      GET\n+/p/#Text/c/#CommentId/tag/#TagId             ProjectCommentTagR       GET POST\n+/p/#Text/c/#CommentId/tag/!apply             ProjectCommentApplyTagR      POST\n+/p/#Text/c/#CommentId/tag/!create            ProjectCommentCreateTagR     POST\n+/p/#Text/d                                   ProjectDiscussionR       GET\n+/p/#Text/d/new                               NewProjectDiscussionR    GET POST\n+/p/#Text/edit                                EditProjectR             GET\n+/p/#Text/feed                                ProjectFeedR             GET\n+/p/#Text/invitation/#Text                    InvitationR              GET POST\n+/p/#Text/invite                              InviteR                  GET POST\n+/p/#Text/patrons                             ProjectPatronsR          GET\n+/p/#Text/button.png                          ProjectPledgeButtonR     GET\n+/p/#Text/shares                              UpdateSharesR            GET POST\n+/p/#Text/t                                   TicketsR                 GET\n+/p/#Text/transactions                        ProjectTransactionsR     GET\n+/p/#Text/volunteer                           VolunteerR               GET POST\n+/p/#Text/w                                   WikiPagesR               GET\n+/p/#Text/who                                 WhoR                     GET\n+/p/#Text/widget                              WidgetR                  GET\n \n /p/#ProjectId/watch   WatchProjectR   POST\n /p/#ProjectId/unwatch UnwatchProjectR POST\n \n -- Project wiki\n \n-/p/#Text/w                                    WikiPagesR           GET\n-/p/#Text/w/#Text                              WikiR                GET POST\n-/p/#Text/w/#Text/d                            DiscussWikiR         GET\n-/p/#Text/w/#Text/d/new                        NewDiscussWikiR      GET POST\n-/p/#Text/w/#Text/diff/#WikiEditId/#WikiEditId WikiDiffR            GET\n-/p/#Text/w/#Text/diffp                        WikiDiffProxyR       GET\n-/p/#Text/w/#Text/edit                         EditWikiR            GET\n-/p/#Text/w/#Text/h                            WikiHistoryR         GET\n-/p/#Text/w/#Text/h/#WikiEditId                WikiEditR            GET\n-/p/#Text/w/#Text/new                          NewWikiR             GET POST\n-/p/#Text/w/#Text/perm                         EditWikiPermissionsR GET POST\n-\n--- Project wiki page comments\n-\n-/p/#Text/w/#Text/c/#CommentId                 DiscussCommentR       GET\n+/p/#Text/w/#Text                              WikiR                 GET POST\n+/p/#Text/w/#Text/c/#CommentId                 WikiCommentR          GET\n /p/#Text/w/#Text/c/#CommentId/close           CloseWikiCommentR     GET POST\n-/p/#Text/w/#Text/c/#CommentId/delete          DeleteCommentR        GET POST DELETE\n-/p/#Text/w/#Text/c/#CommentId/edit            EditCommentR          GET POST\n-/p/#Text/w/#Text/c/#CommentId/flag            FlagCommentR          GET POST\n+/p/#Text/w/#Text/c/#CommentId/delete          DeleteWikiCommentR    GET POST\n+/p/#Text/w/#Text/c/#CommentId/edit            EditWikiCommentR      GET POST\n+/p/#Text/w/#Text/c/#CommentId/flag            FlagWikiCommentR      GET POST\n /p/#Text/w/#Text/c/#CommentId/moderate        ApproveWikiCommentR   GET POST\n-/p/#Text/w/#Text/c/#CommentId/reply           ReplyCommentR         GET POST\n+/p/#Text/w/#Text/c/#CommentId/reply           ReplyWikiCommentR     GET POST\n /p/#Text/w/#Text/c/#CommentId/rethread        RethreadWikiCommentR  GET POST\n /p/#Text/w/#Text/c/#CommentId/retract         RetractWikiCommentR   GET POST\n-/p/#Text/w/#Text/c/#CommentId/tags            CommentTagsR          GET\n-/p/#Text/w/#Text/c/#CommentId/tag/#TagId      CommentTagR           GET POST\n-/p/#Text/w/#Text/c/#CommentId/tag/!new        NewCommentTagR        GET\n-/p/#Text/w/#Text/c/#CommentId/tag/!new/create CreateNewCommentTagR      POST\n-/p/#Text/w/#Text/c/#CommentId/tag/!new/apply  ApplyNewCommentTagR       POST\n-\n--- Experimental, may go away - but there are times when all you have is a CommentId,\n--- and you want to render a page. This function, for now, should redirect to\n--- the correct wiki page. TODO: when we add more discussion types, what then?\n-/c/#CommentId CommentDirectLinkR GET\n+/p/#Text/w/#Text/c/#CommentId/tag/!new        WikiCommentAddTagR    GET\n+/p/#Text/w/#Text/c/#CommentId/tags            WikiCommentTagsR      GET\n+/p/#Text/w/#Text/c/#CommentId/tag/#TagId      WikiCommentTagR       GET POST\n+/p/#Text/w/#Text/c/#CommentId/tag/!apply      WikiCommentApplyTagR      POST\n+/p/#Text/w/#Text/c/#CommentId/tag/!create     WikiCommentCreateTagR     POST\n+/p/#Text/w/#Text/d                            WikiDiscussionR       GET\n+/p/#Text/w/#Text/d/new                        NewWikiDiscussionR    GET POST\n+/p/#Text/w/#Text/diff/#WikiEditId/#WikiEditId WikiDiffR             GET\n+/p/#Text/w/#Text/diffp                        WikiDiffProxyR        GET\n+/p/#Text/w/#Text/edit                         EditWikiR             GET\n+/p/#Text/w/#Text/h                            WikiHistoryR          GET\n+/p/#Text/w/#Text/h/#WikiEditId                WikiEditR             GET\n+/p/#Text/w/#Text/new                          NewWikiR              GET POST\n+/p/#Text/w/#Text/perm                         EditWikiPermissionsR  GET POST\n+\n+/c/#CommentId            CommentDirectLinkR GET      DELETE\n+/c/#CommentId/tag/#TagId CommentTagR        GET\n \n -- Devs only!\n \ndiff --git a/migrations/migrate16 b/migrations/migrate16\nindex bf5a47f..5a88fd5 100644\n--- a/migrations/migrate16\n+++ b/migrations/migrate16\n@@ -20,7 +20,5 @@ CREATe TABLE "event_comment_posted"("id" SERIAL PRIMARY KEY UNIQUE,"comment" INT\n ALTER TABLE "event_comment_posted" ADD CONSTRAINT "event_comment_posted_comment_fkey" FOREIGN KEY("comment") REFERENCES "comment"("id");\n CREATe TABLE "event_comment_pending"("id" SERIAL PRIMARY KEY UNIQUE,"comment" INT8 NOT NULL,"ts" TIMESTAMP NOT NULL);\n ALTER TABLE "event_comment_pending" ADD CONSTRAINT "event_comment_pending_comment_fkey" FOREIGN KEY("comment") REFERENCES "comment"("id");\n-CREATe TABLE "event_message_sent"("id" SERIAL PRIMARY KEY UNIQUE,"message" INT8 NOT NULL,"ts" TIMESTAMP NOT NULL);\n-ALTER TABLE "event_message_sent" ADD CONSTRAINT "event_message_sent_message_fkey" FOREIGN KEY("message") REFERENCES "message"("id");\n CREATe TABLE "event_wiki_edit"("id" SERIAL PRIMARY KEY UNIQUE,"wiki_edit" INT8 NOT NULL,"ts" TIMESTAMP NOT NULL);\n ALTER TABLE "event_wiki_edit" ADD CONSTRAINT "event_wiki_edit_wiki_edit_fkey" FOREIGN KEY("wiki_edit") REFERENCES "wiki_edit"("id");\ndiff --git a/migrations/migrate17 b/migrations/migrate17\nnew file mode 100644\nindex 0000000..1992819\n--- /dev/null\n+++ b/migrations/migrate17\n@@ -0,0 +1 @@\n+DROP TABLE "shares_pledged" CASCADE;\ndiff --git a/migrations/migrate18 b/migrations/migrate18\nnew file mode 100644\nindex 0000000..95ad39d\n--- /dev/null\n+++ b/migrations/migrate18\n@@ -0,0 +1,15 @@\n+ALTER TABLE "pledge" ADD COLUMN "created_ts" TIMESTAMP NOT NULL DEFAULT now();\n+ALTER TABLE "wiki_page" ADD COLUMN "created_ts" TIMESTAMP NOT NULL DEFAULT now();\n+CREATe TABLE "shares_pledged"("id" SERIAL PRIMARY KEY UNIQUE,"ts" TIMESTAMP NOT NULL,"user" INT8 NOT NULL,"project" INT8 NOT NULL,"shares" INT8 NOT NULL,"render" INT8 NOT NULL);\n+ALTER TABLE "shares_pledged" ADD CONSTRAINT "shares_pledged_user_fkey" FOREIGN KEY("user") REFERENCES "user"("id");\n+ALTER TABLE "shares_pledged" ADD CONSTRAINT "shares_pledged_project_fkey" FOREIGN KEY("project") REFERENCES "project"("id");\n+ALTER TABLE "shares_pledged" ADD CONSTRAINT "shares_pledged_render_fkey" FOREIGN KEY("render") REFERENCES "pledge_form_rendered"("id");\n+CREATe TABLE "event_wiki_page"("id" SERIAL PRIMARY KEY UNIQUE,"ts" TIMESTAMP NOT NULL,"wiki_page" INT8 NOT NULL);\n+ALTER TABLE "event_wiki_page" ADD CONSTRAINT "event_wiki_page_wiki_page_fkey" FOREIGN KEY("wiki_page") REFERENCES "wiki_page"("id");\n+CREATe TABLE "event_new_pledge"("id" SERIAL PRIMARY KEY UNIQUE,"ts" TIMESTAMP NOT NULL,"shares_pledged" INT8 NOT NULL);\n+ALTER TABLE "event_new_pledge" ADD CONSTRAINT "event_new_pledge_shares_pledged_fkey" FOREIGN KEY("shares_pledged") REFERENCES "shares_pledged"("id");\n+CREATe TABLE "event_updated_pledge"("id" SERIAL PRIMARY KEY UNIQUE,"ts" TIMESTAMP NOT NULL,"old_shares" INT8 NOT NULL,"shares_pledged" INT8 NOT NULL);\n+ALTER TABLE "event_updated_pledge" ADD CONSTRAINT "event_updated_pledge_shares_pledged_fkey" FOREIGN KEY("shares_pledged") REFERENCES "shares_pledged"("id");\n+CREATe TABLE "event_deleted_pledge"("id" SERIAL PRIMARY KEY UNIQUE,"ts" TIMESTAMP NOT NULL,"user" INT8 NOT NULL,"project" INT8 NOT NULL,"shares" INT8 NOT NULL);\n+ALTER TABLE "event_deleted_pledge" ADD CONSTRAINT "event_deleted_pledge_user_fkey" FOREIGN KEY("user") REFERENCES "user"("id");\n+ALTER TABLE "event_deleted_pledge" ADD CONSTRAINT "event_deleted_pledge_project_fkey" FOREIGN KEY("project") REFERENCES "project"("id");\ndiff --git a/migrations/migrate19 b/migrations/migrate19\nnew file mode 100644\nindex 0000000..9054e60\n--- /dev/null\n+++ b/migrations/migrate19\n@@ -0,0 +1,13 @@\n+DROP TABLE "message" CASCADE;\n+ALTER TABLE "user" RENAME COLUMN "read_messages" TO "read_notifications";\n+CREATe TABLE "user_notification_pref"("id" SERIAL PRIMARY KEY UNIQUE,"user" INT8 NOT NULL,"type" VARCHAR NOT NULL,"delivery" VARCHAR NOT NULL);\n+ALTER TABLE "user_notification_pref" ADD CONSTRAINT "unique_user_notification_pref" UNIQUE("user","type");\n+ALTER TABLE "user_notification_pref" ADD CONSTRAINT "user_notification_pref_user_fkey" FOREIGN KEY("user") REFERENCES "user"("id");\n+CREATe TABLE "notification"("id" SERIAL PRIMARY KEY UNIQUE,"created_ts" TIMESTAMP NOT NULL,"type" VARCHAR NOT NULL,"to" INT8 NOT NULL,"project" INT8 NULL,"content" VARCHAR NOT NULL,"archived" BOOLEAN NOT NULL);\n+ALTER TABLE "notification" ADD CONSTRAINT "notification_to_fkey" FOREIGN KEY("to") REFERENCES "user"("id");\n+ALTER TABLE "notification" ADD CONSTRAINT "notification_project_fkey" FOREIGN KEY("project") REFERENCES "project"("id");\n+CREATe TABLE "unapproved_comment_notification"("id" SERIAL PRIMARY KEY UNIQUE,"comment" INT8 NOT NULL,"notification" INT8 NOT NULL);\n+ALTER TABLE "unapproved_comment_notification" ADD CONSTRAINT "unapproved_comment_notification_comment_fkey" FOREIGN KEY("comment") REFERENCES "comment"("id");\n+ALTER TABLE "unapproved_comment_notification" ADD CONSTRAINT "unapproved_comment_notification_notification_fkey" FOREIGN KEY("notification") REFERENCES "notification"("id");\n+CREATe TABLE "event_notification_sent"("id" SERIAL PRIMARY KEY UNIQUE,"ts" TIMESTAMP NOT NULL,"notification" INT8 NOT NULL);\n+ALTER TABLE "event_notification_sent" ADD CONSTRAINT "event_notification_sent_notification_fkey" FOREIGN KEY("notification") REFERENCES "notification"("id");\ndiff --git a/migrations/migrate20 b/migrations/migrate20\nnew file mode 100644\nindex 0000000..68ab930\n--- /dev/null\n+++ b/migrations/migrate20\n@@ -0,0 +1,2 @@\n+ALTER TABLE "comment_flagging" DROP COLUMN "project_handle";\n+ALTER TABLE "comment_flagging" DROP COLUMN "target";\ndiff --git a/migrations/migrate21 b/migrations/migrate21\nnew file mode 100644\nindex 0000000..53871d4\n--- /dev/null\n+++ b/migrations/migrate21\n@@ -0,0 +1,8 @@\n+ALTER TABLE "user" ADD COLUMN "discussion" INT8 NOT NULL DEFAULT nextval('discussion_id_seq'::regclass);\n+INSERT INTO "discussion" (id, nothing) SELECT "discussion", 0 FROM "user";\n+ALTER TABLE "user" ADD CONSTRAINT "user_discussion_fkey" FOREIGN KEY("discussion") REFERENCES "discussion"("id");\n+ALTER TABLE "project" ADD COLUMN "discussion" INT8 NOT NULL DEFAULT nextval('discussion_id_seq'::regclass);\n+INSERT INTO "discussion" (id, nothing) SELECT "discussion", 0 FROM "project";\n+ALTER TABLE "project" ADD CONSTRAINT "project_discussion_fkey" FOREIGN KEY("discussion") REFERENCES "discussion"("id");\n+ALTER TABLE "project_blog" ADD COLUMN "handle" VARCHAR NOT NULL;\n+ALTER TABLE "project_blog" ADD CONSTRAINT "unique_project_blog_post" UNIQUE("project","handle");\ndiff --git a/templates/application.hamlet b/templates/application.hamlet\nindex c284655..8f7ee76 100644\n--- a/templates/application.hamlet\n+++ b/templates/application.hamlet\n@@ -4,7 +4,7 @@\n             Account:\n         <td>\n             <a href="@{UserR (volunteerApplicationUser application)}">\n-                #{userPrintName user}\n+                #{userDisplayName user}\n \n     <tr>\n         <td>\n@@ -46,12 +46,12 @@\n             <td>\n                 #{experience}\n \n-    $if not (null interests)\n+    $if num_interests /= 0\n         <tr>\n             <td>\n-                #{plural (length interests) "Interest" "Interests"}:\n+                #{plural num_interests "Interest" "Interests"}:\n             <td>\n-                #{rendered_interests}\n+                #{interests}\n \n     $maybe comments <- volunteerApplicationComments application\n         <tr>\ndiff --git a/templates/blog_post.hamlet b/templates/blog_post.hamlet\nindex c031b9e..8bf5cd6 100644\n--- a/templates/blog_post.hamlet\n+++ b/templates/blog_post.hamlet\n@@ -1,6 +1,7 @@\n \n-<h2>\n-    #{title}\n+<div .post>\n+    <h2>\n+        #{title}\n \n-^{content}\n+    ^{content}\n \ndiff --git a/templates/comment.cassius b/templates/comment.cassius\nnew file mode 100644\nindex 0000000..5cb88cd\n--- /dev/null\n+++ b/templates/comment.cassius\n@@ -0,0 +1,79 @@\n+.comment\n+    padding : 0 .8em 0.3em 1em\n+    margin-top : 1.8em\n+    border-bottom-left-radius: 1em\n+    font-size : 15px\n+\n+.comment p, .comment ul, .comment ol, .comment h1, .comment h2, .comment h3, .comment h4, .comment h5, .comment h6\n+    margin : .5em 0\n+\n+.comment figure\n+    text-align : left\n+\n+.comment h1\n+    font-size : large\n+\n+.comment h2\n+    font-size : medium\n+\n+.comment blockquote\n+    font-size : 14px\n+    margin : 1em\n+\n+.comment-head, .comment-action\n+    margin-right : 1em\n+    margin-bottom : 0.5em\n+    display : inline-block\n+    padding : 2px 5px\n+\n+.top_level\n+    border-left : solid black 0.2em\n+\n+.even_depth\n+    border-left : solid lightblue 0.2em\n+\n+.odd_depth\n+    border-left : solid lightgrey 0.2em\n+\n+.small_avatar\n+    width : 2.5em\n+    height : 2.5em\n+    padding : 0em\n+    margin-right : 0.3em\n+    border-radius : 5px\n+\n+.ticket-title\n+    background-color: lightblue;\n+    border-radius: 1em;\n+    padding: 0.5em;\n+\n+.closed\n+    color : goldenrod\n+    border-left : solid goldenrod 0.25em\n+    padding-left : 0.5em\n+\n+.retracted\n+    color : darkred\n+    border-left : solid darkred 0.25em\n+    padding-left : 0.5em\n+\n+.flagged\n+    border : thin solid darkred\n+    border-left : solid red\n+    padding : .5em\n+    margin : .5em 0\n+    display : table\n+\n+.flag-reasons\n+    color : darkred\n+    max-width : 41em\n+\n+.flag-reasons, .flag-markdown\n+    background : whitesmoke\n+    padding : .5em\n+    margin : .5em 0\n+    display : table\n+\n+.preview a\n+    pointer-events: none\n+\ndiff --git a/templates/comment.hamlet b/templates/comment.hamlet\nindex 2229d26..59d79ff 100644\n--- a/templates/comment.hamlet\n+++ b/templates/comment.hamlet\n@@ -5,71 +5,68 @@\n                 <a href="@{UserR user_id}">\n                     <img .small_avatar src="#{author_avatar}"> #\n             <a href="@{UserR user_id}">\n-                #{userPrintName (Entity user_id user)}\n+                #{userDisplayName (Entity user_id user)}\n         <div .comment-head>\n-            $maybe moderated_ts <- commentModeratedTs comment\n-                ^{renderTime moderated_ts}\n+            $maybe approved_ts <- commentApprovedTs comment\n+                ^{renderTime approved_ts}\n             $nothing\n                 ^{renderTime $ commentCreatedTs comment}\n-        <div .comment-head :not show_actions:.preview>\n+        <div .comment-head :is_preview:.preview>\n             |\n-        <div .comment-head :not show_actions:.preview>\n-            <a href="@{DiscussCommentR project_handle target comment_id}">\n+        <div .comment-head :is_preview:.preview>\n+            <a href="@{comment_route_permalink comment_id}">\n                 permalink\n         $maybe parent_id <- commentParent comment\n-            <div .comment-head :not show_actions:.preview>\n+            <div .comment-head :is_preview:.preview>\n                 |\n-            <div .comment-head :not show_actions:.preview>\n-                <a href="@{DiscussCommentR project_handle target parent_id}">\n+            <div .comment-head :is_preview:.preview>\n+                <a href="@{comment_route_permalink parent_id}">\n                     parent\n \n-    <div :not show_actions:.preview>\n+    <div :is_preview:.preview>\n \n         $if can_edit\n             <div .comment-action>\n-                <a href="@{EditCommentR project_handle target comment_id}">\n+                <a href="@{comment_route_edit comment_id}">\n                     edit\n \n         $if can_delete\n             <div .comment-action>\n-                <a href="@{DeleteCommentR project_handle target comment_id}">\n+                <a href="@{comment_route_delete comment_id}">\n                     delete\n \n         $if can_retract\n             <div .comment-action>\n-                <a href="@{RetractWikiCommentR project_handle target comment_id}" style="color: darkred">\n+                <a href="@{comment_route_retract comment_id}" style="color: darkred">\n                     retract\n \n-    $maybe Entity ticket_id _ <- mticket\n-        <div .ticket>SD-#{toPathPiece ticket_id}\n-\n     $forall closure <- earlier_closures\n         $case commentClosureType closure\n             $of Retracted\n                 <div .retracted>\n                     A comment above this one was retracted ^{renderTime (commentClosureTs closure)}\n-                    ^{markdownWidget project_handle (commentClosureReason closure)}\n+                    #{commentClosureReason closure}\n \n             $of Closed\n                 <div .closed>\n                     A comment above this one was closed at ^{renderTime (commentClosureTs closure)}\n-                    ^{markdownWidget project_handle (commentClosureReason closure)}\n+                    #{commentClosureReason closure}\n \n     $maybe closure <- mclosure\n         $case commentClosureType closure\n             $of Retracted\n                 <div .retracted>\n                     The author retracted this comment ^{renderTime (commentClosureTs closure)}\n-                    ^{markdownWidget project_handle (commentClosureReason closure)}\n+                    #{commentClosureReason closure}\n \n             $of Closed\n                 <div .closed>\n                     $if commentClosureClosedBy closure == user_id\n                         The author\n                     $else\n-                        ^{userWidget $ commentClosureClosedBy closure}\n+                        ^{userNameWidget $ commentClosureClosedBy closure}\n                     \\ closed this comment ^{renderTime (commentClosureTs closure)}\n-                    ^{markdownWidget project_handle (commentClosureReason closure)}\n+                    #{commentClosureReason closure}\n \n     $maybe (mflag_markdown, flag_reasons) <- mflag\n         <div .flagged>\n@@ -84,50 +81,49 @@\n                     #{flag_markdown}\n             <i>Please edit to address these concerns and repost.\n \n-    ^{markdownWidget project_handle (commentText comment)}\n+    #{commentText comment}\n \n     <div>\n         $forall tag <- tags\n             ^{tagWidget tag}\n \n-    <div :not show_actions:.preview>\n+    <div :is_preview:.preview>\n \n         $if can_close\n             <div .comment-action>\n-                <a href="@{CloseWikiCommentR project_handle target comment_id}" style="color: goldenrod">\n+                <a href="@{comment_route_close comment_id}" style="color: goldenrod">\n                     close\n \n         $if can_rethread\n             <div .comment-action>\n-                <a href="@{RethreadWikiCommentR project_handle target comment_id}">\n+                <a href="@{comment_route_rethread comment_id}">\n                     rethread\n \n         $if can_add_tag\n             <div .comment-action>\n-                <a href="@{NewCommentTagR project_handle target comment_id}">\n+                <a href="@{comment_route_add_tag comment_id}">\n                     tag\n \n         $if can_flag\n             <div .comment-action>\n-                <a href="@{FlagCommentR project_handle target comment_id}">\n+                <a href="@{comment_route_flag comment_id}">\n                     flag\n \n         $if can_reply\n             <div .comment-action>\n-                <a href="@{ReplyCommentR project_handle target comment_id}">\n+                <a href="@{comment_route_reply comment_id}">\n                     reply\n \n-    <div :not show_actions:.preview>\n-        $if is_mod\n-            $if is_unapproved\n-                <div .comment-action>\n-                    <a href="@{ApproveWikiCommentR project_handle target comment_id}" style="color: green">\n-                        approve\n-\n-            $if can_establish\n-                <div .comment-action>\n-                    <a href="@{UserR user_id}" style="color: green">\n-                        establish user\n+    <div :is_preview:.preview>\n+        $if can_approve\n+            <div .comment-action>\n+                <a href="@{comment_route_approve comment_id}" style="color: green">\n+                    approve\n+\n+        $if can_establish\n+            <div .comment-action>\n+                <a href="@{UserR user_id}" style="color: green">\n+                    establish user\n         $if can_retract\n             $if is_unapproved\n                 <i style="color: green">comment awaiting moderator approval\ndiff --git a/templates/comment_wrapper.cassius b/templates/comment_wrapper.cassius\ndeleted file mode 100644\nindex f4c01b4..0000000\n--- a/templates/comment_wrapper.cassius\n+++ /dev/null\n@@ -1,73 +0,0 @@\n-.comment\n-    padding : 0 .8em 0.3em 1em\n-    margin-top : 1.8em\n-    border-bottom-left-radius: 1em\n-    font-size : 15px\n-\n-.comment p, .comment ul, .comment ol, .comment h1, .comment h2, .comment h3, .comment h4, .comment h5, .comment h6\n-    margin : .5em 0\n-\n-.comment figure\n-    text-align : left\n-\n-.comment h1\n-    font-size : large\n-\n-.comment h2\n-    font-size : medium\n-\n-.comment blockquote\n-    font-size : 14px\n-    margin : 1em\n-\n-.comment-head, .comment-action\n-    margin-right : 1em\n-    margin-bottom : 0.5em\n-    display : inline-block\n-    padding : 2px 5px\n-\n-.top_level\n-    border-left : solid black 0.2em\n-\n-.even_depth\n-    border-left : solid lightblue 0.2em\n-\n-.odd_depth\n-    border-left : solid lightgrey 0.2em\n-\n-.small_avatar\n-    width : 2.5em\n-    height : 2.5em\n-    padding : 0em\n-    margin-right : 0.3em\n-    border-radius : 5px\n-\n-.closed\n-    color : goldenrod\n-    border-left : solid goldenrod 0.25em\n-    padding-left : 0.5em\n-\n-.retracted\n-    color : darkred\n-    border-left : solid darkred 0.25em\n-    padding-left : 0.5em\n-\n-.flagged\n-    border : thin solid darkred\n-    border-left : solid red\n-    padding : .5em\n-    margin : .5em 0\n-    display : table\n-\n-.flag-reasons\n-    color : darkred\n-    max-width : 41em\n-\n-.flag-reasons, .flag-markdown\n-    background : whitesmoke\n-    padding : .5em\n-    margin : .5em 0\n-    display : table\n-\n-.preview a\n-    pointer-events: none\ndiff --git a/templates/comment_wrapper.hamlet b/templates/comment_wrapper.hamlet\ndeleted file mode 100644\nindex f05cd84..0000000\n--- a/templates/comment_wrapper.hamlet\n+++ /dev/null\n@@ -1,9 +0,0 @@\n-<div .row>\n-    <div .col-xs-6>\n-        <a href="@{DiscussWikiR project_handle target}"> back to full discussion\n-    <div .col-xs-6>\n-        <a href="@{WikiR project_handle target}"> back to wiki page\n-\n-<hr .wikitop>\n-\n-^{comment_widget}\ndiff --git a/templates/default-layout.cassius b/templates/default-layout.cassius\nindex 9c68925..d1d702a 100644\n--- a/templates/default-layout.cassius\n+++ b/templates/default-layout.cassius\n@@ -12,10 +12,10 @@ img\n     max-width: 100%\n     height: auto\n \n-input, textarea\n+input, textarea, button\n     font-size : 15px\n \n-input[type=submit]\n+input[type=submit], button\n     color : white\n     font-weight : bold\n     padding : .4em 1em\n@@ -27,11 +27,11 @@ input[type=submit]\n     background : #74BBFB\n     background-image : linear-gradient(#92D9FF, #62AAEA)\n \n-input[type=submit]:hover, input[type=submit]:focus\n+input[type=submit]:hover, input[type=submit]:focus, button:hover, button:focus\n     background : #62AAEA\n     background-image : linear-gradient(#78BFDD, #6097D9)\n \n-input[type=submit]:active\n+input[type=submit]:active, button:active\n     background : #62AAEA\n     background-image : linear-gradient(#78BFDD, #6097D9)\n     box-shadow : 0 3px 5px rgba(0, 0, 0, 0.3) inset\n@@ -71,9 +71,21 @@ hr\n     color : #CCCCDD\n     background-color : #CCCCDD\n \n-hr.wikitop\n-    margin-top : 0.1em\n-    padding-bottom : 0.1em\n+.page-toolbox\n+    float : right\n+    border : solid lightgrey\n+    margin-left : 1em\n+\n+/* This @media sets the page-toolbox above the page for small screens */\n+@media (max-width: 767px)\n+    .page-toolbox\n+        float : none\n+        display : inline-flex\n+        margin-left : 0\n+        \n+.page-tool\n+    margin : 8px\n+    padding : 2px 5px\n \n footer\n     text-align : center\n@@ -143,8 +155,8 @@ h1, h2, h3, h4, h5, h6\n     margin : 1.6em 0 0.8em\n     max-width : 780px\n \n-h1:first-child\n-    margin-top : 0\n+h1:first-of-type\n+    margin-top : .5em\n \n q:before\n     content : '"'\ndiff --git a/templates/homepage.hamlet b/templates/homepage.hamlet\nindex a30c713..10a7f32 100644\n--- a/templates/homepage.hamlet\n+++ b/templates/homepage.hamlet\n@@ -42,7 +42,7 @@\n <p>\n     If your\n     <a href="@{WikiR "snowdrift" "project-requirements"}">free/libre/open\n-    project needs funding, please <a href=@{ContactR "snowdrift"}>contact us</a>.\n+    project needs funding, please contact us. TODO(aaron): where should this link?\n     We want projects included as we develop to make sure things work well for you!\n \n <h2>Donate\n@@ -77,7 +77,7 @@\n             <img alt="Bitcoin" src="/static/img/external/Bitcoin.png" style="height: 4em"> \n         <small>\n             We're still deciding the best options, but, for now,\n-            <a href=@{ContactR "snowdrift"}>contact us\n+            contact us (TODO(aaron): where should this link?)\n             to get a BTC invoice with an address to donate.\n \n <small>\n@@ -88,7 +88,7 @@\n     <p>\n         <em>\n             Please note:\n-            Snowdrift.coop has incorporated as a Michigan non-profit cooperative,\n-            and we are studying our fit for federal 501(c)(4) status.\n+            Snowdrift.coop is incorporated as Michigan non-profit cooperative,\n+            but we do not have federal tax-exempt status.\n             Unlike a 501(c)(3) charity, donations to us are not tax-deductible.\n \ndiff --git a/templates/messages.hamlet b/templates/messages.hamlet\ndeleted file mode 100644\nindex fe7cca5..0000000\n--- a/templates/messages.hamlet\n+++ /dev/null\n@@ -1,31 +0,0 @@\n-$if not (null messages)\n-    $forall Entity _ message <- messages\n-        <div .row>\n-            <div .col-sm-3 .well .well-sm>\n-                $if (messageAutomated message)\n-                    <em>automated message\n-                    <br>\n-                $maybe from <- messageFrom message\n-                    <em>from\n-                    #{getUserName from}\n-                $nothing\n-                    $maybe project <- messageProject message\n-                        <em>from\n-                        ^{projectNameWidget project}\n-                    $nothing\n-                        <em>from unregistered user\n-                <br>\n-                $maybe to <- messageTo message\n-                    <em>to\n-                    #{getUserName to}\n-                $nothing\n-                    $maybe project <- messageProject message\n-                        <em>to\n-                        ^{projectNameWidget project}\n-                    $nothing\n-                        ERROR! How can there be a message that isn't to anyone?\n-                <br>\n-                ^{renderTime (messageCreatedTs message)}\n-\n-            <div .col-sm-9>\n-                #{messageContent message}\ndiff --git a/templates/navbar.hamlet b/templates/navbar.hamlet\nindex f2bd1e5..059ffa6 100644\n--- a/templates/navbar.hamlet\n+++ b/templates/navbar.hamlet\n@@ -7,14 +7,6 @@\n         <ul .nav .navbar-nav .navbar-left>\n             <li>\n                 <a href=@{ProjectsR}> Browse Projects\n-            $maybe _ <- maybe_user\n-                <li>\n-                    <p .navbar-text .text-center>\n-                        <a href=@{ProjectsR}>\n-                            new activity\n-                            <br>\n-                            <span .badge>\n-                                0 TODO\n         <ul .nav .navbar-nav .navbar-right>\n             $maybe Entity user_id user <- maybe_user\n                 <li>\n@@ -28,10 +20,10 @@\n                             <a .navbar-link title="Current balance" href="@{UserBalanceR user_id}">\n                                 #{milrayCents balance}\n                 <li>\n-                    <a href=@{MessagesR}>\n-                        $if not (null messages)\n+                    <a href=@{NotificationsR}>\n+                        $if num_unread_notifs /= 0\n                             <span .glyphicon .glyphicon-envelope>&nbsp;\n-                                #{length messages}\n+                                #{num_unread_notifs}\n                         $else\n                             <span .glyphicon .glyphicon-envelope>\n                 $with name <- fromMaybe (userIdent user) $ userName user \n@@ -71,10 +63,10 @@\n         <ul .nav .nav-pills .navbar-right>\n             $maybe Entity user_id user <- maybe_user\n                 <li>\n-                    <a href="@{MessagesR}">\n-                        $if not (null messages)\n+                    <a href="@{NotificationsR}">\n+                        $if num_unread_notifs /= 0\n                             <span .glyphicon .glyphicon-envelope>&nbsp;\n-                                #{length messages}\n+                                #{num_unread_notifs}\n                         $else\n                             <span .glyphicon .glyphicon-envelope>\n                 $with name <- fromMaybe (userIdent user) $ userName user \ndiff --git a/templates/new_blog_post.hamlet b/templates/new_blog_post.hamlet\nnew file mode 100644\nindex 0000000..e1a3191\n--- /dev/null\n+++ b/templates/new_blog_post.hamlet\n@@ -0,0 +1,5 @@\n+<form method=POST>\n+    ^{blog_form}\n+\n+    <input type=submit name=mode value=preview>\n+    <input type=submit name=mode value=post>\ndiff --git a/templates/new_comment_tag.hamlet b/templates/new_comment_tag.hamlet\nindex c59cf71..b4c6687 100644\n--- a/templates/new_comment_tag.hamlet\n+++ b/templates/new_comment_tag.hamlet\n@@ -1,8 +1,10 @@\n-<form class="tagcloud" method=post action="./new/apply">\n+$# TODO: We should use type-safe URLs here... just got bit by this...\n+\n+<form class="tagcloud" method=post action="./apply">\n     ^{apply_form}\n     <input type=submit value="apply">\n \n-<form method=post action="./new/create">\n+<form method=post action="./create">\n     ^{create_form}\n     <input type=submit value="create & apply">\n $forall tag <- tags\ndiff --git a/templates/notifications.hamlet b/templates/notifications.hamlet\nnew file mode 100644\nindex 0000000..c42ee2c\n--- /dev/null\n+++ b/templates/notifications.hamlet\n@@ -0,0 +1,14 @@\n+$if not (null notifs)\n+    $forall Entity _ notif <- notifs\n+        <div .row>\n+            <div .col-sm-3 .well .well-sm>\n+\n+                $maybe project <- notificationProject notif\n+                    <em>^{projectNameWidget project}\n+\n+                <br>\n+\n+                ^{renderTime (notificationCreatedTs notif)}\n+\n+            <div .col-sm-9>\n+                #{notificationContent notif}\ndiff --git a/templates/project_blog.hamlet b/templates/project_blog.hamlet\nindex 22cba0c..0797f41 100644\n--- a/templates/project_blog.hamlet\n+++ b/templates/project_blog.hamlet\n@@ -1,11 +1,18 @@\n-$forall Entity post_id post <- posts\n-    <div>\n-        <a href=@{ProjectBlogPostR project_handle post_id}>\n+$forall Entity _ post <- posts\n+    <div .post>\n+        <a href=@{ProjectBlogPostR project_handle (projectBlogHandle post)}>\n             #{projectBlogTitle post}\n \n+        \\ - #\n+\n+        <small>\n+            ^{renderTime $ projectBlogTime post}\n+\n         <p>\n             ^{markdownWidget project_handle $ projectBlogTopContent post}\n \n+    <hr>\n+\n $case next\n     $of [Entity next_id _]\n         <a href=#{nextRoute next_id}>\ndiff --git a/templates/project_discuss.hamlet b/templates/project_discuss.hamlet\nnew file mode 100644\nindex 0000000..259380e\n--- /dev/null\n+++ b/templates/project_discuss.hamlet\n@@ -0,0 +1,22 @@\n+<div .page-toolbox>\n+    <div .page-tool>\n+        <a href="@{ProjectR project_handle}"> back to main project page\n+    $if has_comments\n+        <div .page-tool>\n+            <a href="@{NewProjectDiscussionR project_handle}"> new topic\n+    <div .page-tool>\n+        <a href="@{ProjectDiscussionR project_handle}?state=closed">\n+            closed threads\n+\n+^{comment_forest}\n+\n+$if not has_comments\n+    $if isJust muser\n+        <form action="@{NewProjectDiscussionR project_handle}" method="POST">\n+            ^{comment_form}\n+            <input type="submit" name="mode" value="preview">\n+    $else\n+        <p>\n+            There is no discussion here yet.\n+            <a href=@{AuthR LoginR}>Sign in\n+            to start a new topic.\ndiff --git a/templates/project_discussion_wrapper.hamlet b/templates/project_discussion_wrapper.hamlet\nnew file mode 100644\nindex 0000000..ac38f83\n--- /dev/null\n+++ b/templates/project_discussion_wrapper.hamlet\n@@ -0,0 +1,5 @@\n+<div .page-toolbox>\n+    <div .page-tool>\n+        <a href="@{ProjectDiscussionR project_handle}"> back to full discussion\n+\n+^{widget}\ndiff --git a/templates/project_feed.cassius b/templates/project_feed.cassius\nnew file mode 100644\nindex 0000000..3e2631f\n--- /dev/null\n+++ b/templates/project_feed.cassius\n@@ -0,0 +1,5 @@\n+.event\n+    padding : 5px    \n+    max-width : 42em\n+    margin : auto\n+    border-top : thin solid\ndiff --git a/templates/project_feed.hamlet b/templates/project_feed.hamlet\nindex a94f027..a729933 100644\n--- a/templates/project_feed.hamlet\n+++ b/templates/project_feed.hamlet\n@@ -1,4 +1,4 @@\n-<h1>#{project_handle} feed.\n+<h1>#{projectName project} activity feed:\n \n $forall event <- events\n     $case event\n@@ -8,24 +8,42 @@ $forall event <- events\n             $# on by looking in each map in order. There ought to be a nicer\n             $# way to do this, but I'd like to keep both of:\n             $#   - ECommentPosted not being split into a different event for\n-            $#       each location a comment might be posted.\n+            $#       each comment location.\n             $#   - A single event data type (as opposed to something like \n             $#       FeedEvent that specialized SnowdriftEvent, e.g.\n             $#       FECommentPostedOnWikiPage)\n \n-            $maybe wiki_page <- M.lookup (commentDiscussion comment) discussion_wiki_pages_map\n-                ^{renderCommentPostedOnWikiPageEvent comment_id comment wiki_page}\n+            $if commentDiscussion comment == projectDiscussion project\n+                ^{renderProjectCommentPostedEvent comment_id comment project muser_id earlier_closures_map user_map closure_map ticket_map flag_map}\n+            $else\n+                $maybe wiki_page <- M.lookup (commentDiscussion comment) discussion_wiki_page_map\n+                    ^{renderWikiPageCommentPostedEvent comment_id comment wiki_page project_handle muser_id earlier_closures_map user_map closure_map ticket_map flag_map}\n \n-            $# This should never happen.\n+                $# This should never happen.\n \n-            $nothing\n-                ^{renderCommentPostedOnUnknownDiscussionEvent comment_id comment}\n+                $nothing\n+                    ^{renderCommentPostedOnUnknownDiscussionEvent comment_id comment}\n+\n+        $of EWikiPage wiki_page_id wiki_page\n+            ^{renderWikiPageEvent project_handle wiki_page_id wiki_page user_map}\n \n         $of EWikiEdit wiki_edit_id wiki_edit\n-            ^{renderWikiEditEvent wiki_edit_id wiki_edit (Entity (wikiEditPage wiki_edit) (fromJust $ M.lookup (wikiEditPage wiki_edit) wiki_pages_map))}\n+            ^{renderWikiEditEvent project_handle wiki_edit_id wiki_edit wiki_page_map user_map}\n+\n+        $of ENewPledge shares_pledged_id shares_pledged\n+            ^{renderNewPledgeEvent shares_pledged_id shares_pledged user_map}\n+\n+        $of EUpdatedPledge old_shares shares_pledged_id shares_pledged\n+            ^{renderUpdatedPledgeEvent old_shares shares_pledged_id shares_pledged user_map}\n+\n+        $of EDeletedPledge ts user_id _ shares\n+            ^{renderDeletedPledgeEvent ts user_id shares user_map}\n+\n+        $# Graveyard of event types we don't want to put on the feed.\n+        $# Don't match-all here, we don't want to accidentally not consider something.\n \n+        $of ENotificationSent _ _\n         $of ECommentPending _ _\n-            $# TODO?\n \n-        $of EMessageSent _ _\n-            $# TODO?\n+$maybe next_before <- mnext_before\n+    <a href=@?{(ProjectFeedR project_handle, [("before", next_before)])}>next page\ndiff --git a/templates/project_patrons.hamlet b/templates/project_patrons.hamlet\nindex a07ff17..68546a5 100644\n--- a/templates/project_patrons.hamlet\n+++ b/templates/project_patrons.hamlet\n@@ -9,7 +9,7 @@ $else\n             <tr>\n                 <td>\n                     <a href="@{UserR (entityKey user)}">\n-                        #{userPrintName user}\n+                        #{userDisplayName user}\n                 <td>\n                     #{show (pledgeFundedShares (entityVal pledge))}&nbsp;shares\n                 <td>\ndiff --git a/templates/project_transactions.hamlet b/templates/project_transactions.hamlet\nindex c6ca557..163eaec 100644\n--- a/templates/project_transactions.hamlet\n+++ b/templates/project_transactions.hamlet\n@@ -34,7 +34,7 @@ $else\n                                     $case M.lookup other_account_id account_map\n                                         $of Just (Right user)\n                                             <a href=@{UserR $ entityKey user}>\n-                                                #{userPrintName user}\n+                                                #{userDisplayName user}\n \n                                         $of Just (Left project)\n                                             <a href=@{ProjectR $ projectHandle $ entityVal project}>\ndiff --git a/templates/projects.hamlet b/templates/projects.hamlet\nindex 22e1cf1..105b2e8 100644\n--- a/templates/projects.hamlet\n+++ b/templates/projects.hamlet\n@@ -4,9 +4,11 @@ $else\n     <table .projects>\n         <tr>\n             <th> Project\n-            <th> Edits\n+            <th> Feed\n             <th> Discussion\n+            <th> Wiki\n             <th> Share value\n+\n         $forall Entity _ project <- projects\n             <tr>\n                 <td>\n@@ -14,6 +16,12 @@ $else\n                         #{projectName project}\n                 <td>\n                     <a href=@{ProjectFeedR (projectHandle project)}>\n-                        <span .glyphicon .glyphicon-list-alt>&nbsp;\n+                        <span .glyphicon .glyphicon-list>&nbsp;\n+                <td>\n+                    <a href=@{ProjectDiscussionR (projectHandle project)}>\n+                        <span .glyphicon .glyphicon-comment>&nbsp;\n+                <td>\n+                    <a href=@{WikiPagesR (projectHandle project)}>\n+                        <span .glyphicon .glyphicon-book>&nbsp;\n                 <td>\n                     #{show (projectShareValue project)}/share\ndiff --git a/templates/rethread.hamlet b/templates/rethread.hamlet\ndeleted file mode 100644\nindex 20d8d19..0000000\n--- a/templates/rethread.hamlet\n+++ /dev/null\n@@ -1,4 +0,0 @@\n-<form method=post>\n-    ^{form}\n-    <input type=submit name=mode value=rethread>\n-\ndiff --git a/templates/tag.hamlet b/templates/tag.hamlet\nindex 03fa1fb..81efe14 100644\n--- a/templates/tag.hamlet\n+++ b/templates/tag.hamlet\n@@ -13,7 +13,7 @@ $else\n             <tr>\n                 <td>\n                     <a href=@{UserR (entityKey user)}>\n-                        #{userPrintName user}\n+                        #{userDisplayName user}\n \n                 <td>\n                     #{show votes}\ndiff --git a/templates/tags.hamlet b/templates/tags.hamlet\nindex 6b2baaf..5abae7e 100644\n--- a/templates/tags.hamlet\n+++ b/templates/tags.hamlet\n@@ -5,20 +5,19 @@ $else\n         $forall tag <- tags\n             <tr>\n                 <td>\n-                    #{atName tag}\n+                    #{annotTagName tag}\n \n                 <td>\n \n                 <td>\n-                    #{atScoreString tag}\n+                    #{annotTagScoreString tag}\n \n-            $forall (user, votes) <- atUserVotes tag\n+            $forall (user, votes) <- annotTagUserVotes tag\n                 <tr>\n                     <td>\n \n                     <td>\n-                        #{userPrintName user}\n+                        #{userDisplayName user}\n \n                     <td>\n                         #{show votes}\n-\ndiff --git a/templates/tickets.hamlet b/templates/tickets.hamlet\nindex 5e06979..a726e9d 100644\n--- a/templates/tickets.hamlet\n+++ b/templates/tickets.hamlet\n@@ -2,7 +2,7 @@\n     ^{formWidget}\n     <input type=submit value="update view">\n \n-$if null tickets\n+$if null issues\n     <p> no tickets to display\n $else\n     <table>\ndiff --git a/templates/user.hamlet b/templates/user.hamlet\nindex f358634..58581bf 100644\n--- a/templates/user.hamlet\n+++ b/templates/user.hamlet\n@@ -1,7 +1,7 @@\n <div .row>\n     <div .col-md-6>\n         <h1>\n-            #{userPrintName user_entity}\n+            #{userDisplayName user_entity}\n         <h5>\n             $if Just user_id == mviewer_id\n                 <a href="@{EditUserR user_id}">\n@@ -10,7 +10,7 @@\n             <div .col-xs-4>\n                 $maybe avatar <- userAvatar user\n                     <img .headshot_large src="#{avatar}">\n-                $if isEstablished user\n+                $if userIsEstablished user\n                     <small>established user\n             <div .col-xs-8>\n                 $maybe nick <- userIrcNick user\n@@ -58,11 +58,14 @@ $maybe (est_form, est_form_enctype) <- mest_form_and_enctype\n             <h4>Establish User\n             <p>\n                 As a project moderator, you have the ability to "establish" ordinary users.\n-                Established users have additional permissions such as flagging, tagging, and commenting without moderation.\n+                Established users have additional permissions such as\n+                flagging, tagging, and commenting without moderation.\n             <p>\n                 Approve users who you now trust to be legitimate\n-                (e.g. they have posted several helpful, on-topic comments and are clearly not a spammer or malicious troll).\n-                They will then get fully established after affirming the <a href="@{WikiR "snowdrift" "honor"}">honor pledge</a>.\n+                (e.g. they have posted several helpful, on-topic comments,\n+                and are clearly not a spammer or malicious troll).\n+                qThey will then get fully established after affirming\n+                the <a href="@{WikiR "snowdrift" "honor"}">honor pledge</a>.\n \n             <form action=@{UserEstEligibleR user_id} method=post enctype=#{est_form_enctype}>\n                 ^{est_form}\ndiff --git a/templates/users.hamlet b/templates/users.hamlet\nindex 7235449..33ca527 100644\n--- a/templates/users.hamlet\n+++ b/templates/users.hamlet\n@@ -20,12 +20,12 @@\n                 <tr>\n                     <td>\n                         <a href=@{UserR $ entityKey user}>\n-                            #{userPrintName user}\n+                            #{userDisplayName user}\n                     <td>\n                         $maybe nick <- userIrcNick $ entityVal user\n                             #{nick}\n                     <td>\n-                        $if isEstablished (entityVal user)\n+                        $if userIsEstablished (entityVal user)\n                             &#10004;\n                     <td>\n                         $forall ((projectName, projectHandle), roles) <- M.toAscList (fromMaybe mempty (userProjects user))\ndiff --git a/templates/volunteer.hamlet b/templates/volunteer.hamlet\nindex a8b02fe..517c76b 100644\n--- a/templates/volunteer.hamlet\n+++ b/templates/volunteer.hamlet\n@@ -4,10 +4,6 @@\n     Please read the\n     <a href=@{WikiR project_handle "how-to-help"}>How-to-help page\n     to find out more about our particular needs at this time.\n-<p>\n-    To provide general feedback or request assistance, go to the\n-    <a href=@{ContactR project_handle}>Contact page\n-    or participate in the discussions connected with each wiki page.\n \n <p>\n     To express interest in volunteering, fill out this form:\ndiff --git a/templates/wiki.hamlet b/templates/wiki.hamlet\nindex 4cefe99..5941e77 100644\n--- a/templates/wiki.hamlet\n+++ b/templates/wiki.hamlet\n@@ -1,24 +1,14 @@\n-$if can_edit\n-    <div .row>\n-        <div .col-sm-4>\n+<div .page-toolbox>\n+    $if can_edit\n+        <div .page-tool>\n             <a href="@{EditWikiR project_handle target}">edit\n-        <div .col-sm-4>\n-            <a href="@{WikiHistoryR project_handle target}">history\n-        <div .col-sm-4>\n-            <a href="@{DiscussWikiR project_handle target}">\n-                discuss\n-                <span .badge>\n-                    #{comment_count}\n-$else\n-    $if can_view_meta\n-        <div .row>\n-            <div .col-xs-6>\n-                <a href="@{WikiHistoryR project_handle target}">history\n-            <div .col-xs-6>\n-                <a href="@{DiscussWikiR project_handle target}">\n-                    discussion\n-                    <span .badge>\n-                        #{comment_count}\n-<hr .wikitop>\n+    <div .page-tool>\n+        <a href="@{WikiHistoryR project_handle target}">history\n+    <div .page-tool>\n+        <a href="@{WikiDiscussionR project_handle target}">\n+            discuss\n+            <span .badge>\n+                #{comment_count}\n+\n ^{markdownWidget project_handle (wikiPageContent page)}\n \ndiff --git a/templates/wiki_diff.hamlet b/templates/wiki_diff.hamlet\nindex 190a1ff..cdbce86 100644\n--- a/templates/wiki_diff.hamlet\n+++ b/templates/wiki_diff.hamlet\n@@ -1,13 +1,12 @@\n $# TODO - info from compared versions\n \n-<div .row>\n-    <div .col-sm-4> \n+<div .page-toolbox>\n+    <div .page-tool> \n         <a href="@{WikiHistoryR project_handle target}">back to history \n-    <div .col-sm-4>\n+    <div .page-tool>\n         <a href="@{WikiR project_handle target}">current version\n-    <div .col-sm-4>\n-        <a href="@{DiscussWikiR project_handle target}">view discussion\n+    <div .page-tool>\n+        <a href="@{WikiDiscussionR project_handle target}">view discussion\n \n-<hr .wikitop>\n #{renderDiff (diffEdits start_edit end_edit)}\n \ndiff --git a/templates/wiki_discuss.hamlet b/templates/wiki_discuss.hamlet\nindex bb1e059..2e2ba81 100644\n--- a/templates/wiki_discuss.hamlet\n+++ b/templates/wiki_discuss.hamlet\n@@ -1,19 +1,18 @@\n-<div .row>\n-    <div .col-sm-4>\n+<div .page-toolbox>\n+    <div .page-tool>\n         <a href="@{WikiR project_handle target}"> back to wiki page\n     $if has_comments\n-        <div .col-sm-4>\n-            <a href="@{NewDiscussWikiR project_handle target}"> new topic\n-    <div .col-sm-4>\n-        <a href="@{DiscussWikiR project_handle target}?state=closed">\n+        <div .page-tool>\n+            <a href="@{NewWikiDiscussionR project_handle target}"> new topic\n+    <div .page-tool>\n+        <a href="@{WikiDiscussionR project_handle target}?state=closed">\n             closed threads\n-<hr .wikitop>\n \n-^{comments}\n+^{comment_forest}\n \n $if not has_comments\n     $if isJust muser\n-        <form action="@{NewDiscussWikiR project_handle target}" method="POST">\n+        <form action="@{NewWikiDiscussionR project_handle target}" method="POST">\n             ^{comment_form}\n             <input type="submit" name="mode" value="preview">\n     $else\ndiff --git a/templates/wiki_discuss_new.hamlet b/templates/wiki_discuss_new.hamlet\ndeleted file mode 100644\nindex 6b20853..0000000\n--- a/templates/wiki_discuss_new.hamlet\n+++ /dev/null\n@@ -1,8 +0,0 @@\n-<div .row>\n-    <div .col-xs-6>\n-        <a href="@{WikiR project_handle target}"> back to wiki page\n-<hr .wikitop>\n-\n-<form action="@{NewDiscussWikiR project_handle target}" method="POST">\n-    ^{comment_form}\n-    <input type="submit" name="mode" value="preview">\ndiff --git a/templates/wiki_discussion_wrapper.hamlet b/templates/wiki_discussion_wrapper.hamlet\nnew file mode 100644\nindex 0000000..61d04c3\n--- /dev/null\n+++ b/templates/wiki_discussion_wrapper.hamlet\n@@ -0,0 +1,7 @@\n+<div .page-toolbox>\n+    <div .page-tool>\n+        <a href="@{WikiDiscussionR project_handle target}"> back to full discussion\n+    <div .page-tool>\n+        <a href="@{WikiR project_handle target}"> back to wiki page\n+\n+^{widget}\ndiff --git a/templates/wiki_edit.hamlet b/templates/wiki_edit.hamlet\nindex 3ad2e42..2a32d91 100644\n--- a/templates/wiki_edit.hamlet\n+++ b/templates/wiki_edit.hamlet\n@@ -4,14 +4,14 @@\n     Edit comment: #\n     $maybe comment <- wikiEditComment edit\n         #{comment}\n+<hr>\n \n-<div .row>\n-    <div .col-sm-4>\n+<div .page-toolbox>\n+    <div .page-tool>\n         <a href="@{WikiHistoryR project_handle target}">back to history\n-    <div .col-sm-4>\n+    <div .page-tool>\n         <a href="@{WikiR project_handle target}">current version\n-    <div .col-sm-4>\n-        <a href="@{DiscussWikiR project_handle target}">view discussion\n+    <div .page-tool>\n+        <a href="@{WikiDiscussionR project_handle target}">view discussion\n \n-<hr .wikitop>\n ^{markdownWidget project_handle (wikiEditContent edit)}\ndiff --git a/templates/wiki_history.hamlet b/templates/wiki_history.hamlet\nindex 229ffa9..3d0c18b 100644\n--- a/templates/wiki_history.hamlet\n+++ b/templates/wiki_history.hamlet\n@@ -1,7 +1,5 @@\n-<div .row>\n-    <div .col-xs-6>\n-        <a href="@{WikiR project_handle target}"> back to current page\n-<hr .wikitop>\n+<a href="@{WikiR project_handle target}"> back to current page\n+\n <form action="@{WikiDiffProxyR project_handle target}" method="GET">\n     <table>\n         <tr>\n@@ -17,7 +15,7 @@\n                     $with user_id <- wikiEditUser edit\n                         $maybe user <- M.lookup user_id users\n                             <a href="@{UserR user_id}">\n-                                #{userPrintName user}\n+                                #{userDisplayName user}\n                 <td>\n                     $maybe comment <- wikiEditComment edit\n                         #{comment}\ndiff --git a/templates/wiki_new_comments.hamlet b/templates/wiki_new_comments.hamlet\ndeleted file mode 100644\nindex 10bd251..0000000\n--- a/templates/wiki_new_comments.hamlet\n+++ /dev/null\n@@ -1,24 +0,0 @@\n-$if isJust mviewer\n-    $if not (null unapproved_comments)\n-        <h3>\n-            unapproved comments\n-        ^{rendered_unapproved_comments}\n-\n-    $if null new_comments\n-        <h3>\n-            no new comments\n-    $else\n-        <h3>\n-            new comments\n-        ^{rendered_new_comments}\n-    $if not (null old_comments)\n-        <hr>\n-        <h3>\n-            old comments\n-        ^{rendered_old_comments}\n-$else\n-    ^{rendered_new_comments}\n-    ^{rendered_old_comments}\n-$if show_older\n-    <a href="@{WikiNewCommentsR project_handle}?from=#{show (to - 1)}&since=#{show since}">\n-        older comments\ndiff --git a/templates/wiki_new_edits.hamlet b/templates/wiki_new_edits.hamlet\ndeleted file mode 100644\nindex ac9da51..0000000\n--- a/templates/wiki_new_edits.hamlet\n+++ /dev/null\n@@ -1,22 +0,0 @@\n-$if isJust mauth\n-    $if null new_edits'\n-        no new edits\n-    $else\n-        new edits\n-        <table>\n-            $forall edit <- new_edits'\n-                ^{renderEdit edit}\n-    $if not (null old_edits')\n-        <hr>\n-        old edits\n-        <table>\n-            $forall edit <- old_edits'\n-                ^{renderEdit edit}\n-$else\n-    <table>\n-        $forall edit <- new_edits' <> old_edits'\n-            ^{renderEdit edit}\n-$if show_older \n-    <a href="@{WikiNewEditsR project_handle}?from=#{show (to - 1)}">\n-        older edits\n-\ndiff --git a/templates/wiki_pages.hamlet b/templates/wiki_pages.hamlet\nindex 50ded82..06f450f 100644\n--- a/templates/wiki_pages.hamlet\n+++ b/templates/wiki_pages.hamlet\n@@ -1,13 +1,20 @@\n $if null pages\n     no pages to display\n $else\n-    <table>\n+    <table .wiki-pages>\n         <tr>\n-            <th>\n-                page URL\n+            <th>Title\n+            <th>Discussion\n+            <th>History\n \n         $forall Entity _ page <- pages\n             <tr>\n                 <td>\n                     <a href=@{WikiR project_handle (wikiPageTarget page)}>\n                         #{wikiPageTarget page}\n+                <td>\n+                    <a href=@{WikiDiscussionR project_handle (wikiPageTarget page)}>\n+                        <span .glyphicon .glyphicon-comment>&nbsp;\n+                <td>\n+                    <a href=@{WikiHistoryR project_handle (wikiPageTarget page)}>\n+                        <span .glyphicon .glyphicon-list-alt>&nbsp;\ndiff --git a/tests/BlogTest.hs b/tests/BlogTest.hs\nnew file mode 100644\nindex 0000000..fb02f7d\n--- /dev/null\n+++ b/tests/BlogTest.hs\n@@ -0,0 +1,155 @@\n+{-# LANGUAGE OverloadedStrings #-}\n+{-# LANGUAGE FlexibleContexts #-}\n+\n+module BlogTest\n+    ( blogSpecs\n+    ) where\n+\n+import TestImport\n+import qualified Data.Map as M\n+import qualified Text.XML as XML\n+import qualified Text.HTML.DOM as HTML\n+\n+import Database.Esqueleto hiding (get)\n+\n+import Network.Wai.Test (SResponse (..))\n+import Data.Text as T\n+import Data.Text.Encoding\n+import qualified Data.ByteString.Char8 as BSC\n+\n+import Control.Monad\n+\n+import Data.Maybe (fromMaybe)\n+\n+blogSpecs :: Spec\n+blogSpecs = do\n+    let postBlog route stmts = [marked|\n+            get route\n+            statusIs 200\n+\n+            [ form ] <- htmlQuery "form"\n+\n+            let getAttrs = XML.elementAttributes . XML.documentRoot . HTML.parseLBS\n+\n+            request $ do\n+                addNonce\n+                setMethod "POST"\n+                let route' = maybe (Left route) Right $ M.lookup "action" $ getAttrs form\n+                either setUrl setUrl route'\n+                addPostParam "mode" "post"\n+                stmts\n+\n+            statusIsResp 302\n+        |]\n+\n+        previewBlog route stmts = [marked|\n+            get route\n+            statusIs 200\n+\n+            [ form ] <- htmlQuery "form"\n+\n+            let getAttrs = XML.elementAttributes . XML.documentRoot . HTML.parseLBS\n+\n+            request $ do\n+                addNonce\n+                setMethod "POST"\n+                maybe (setUrl route) setUrl $ M.lookup "action" $ getAttrs form\n+\n+                addPostParam "mode" "preview"\n+                stmts\n+\n+            statusIs 200\n+        |]\n+\n+\n+\n+    ydescribe "blog" $ do\n+\n+        yit "loads the project page - no blog post" $ [marked|\n+            login\n+\n+            get $ ProjectR "snowdrift"\n+\n+            statusIs 200\n+\n+        {-\n+            htmlNoneContain "#post" "Above fold."\n+            htmlNoneContain "#post" "Below fold."\n+        -}\n+        |]\n+\n+\n+        yit "loads the project blog - no blog post" $ [marked|\n+            login\n+\n+            get $ ProjectBlogR "snowdrift"\n+\n+            statusIs 200\n+\n+            htmlNoneContain ".post" "Above fold."\n+            htmlNoneContain ".post" "Below fold."\n+        |]\n+\n+\n+        yit "previews blog post" $ [marked|\n+            login\n+\n+            previewBlog (NewProjectBlogPostR "snowdrift") $ do\n+                byLabel "Post Title" "Test"\n+                byLabel "Post Handle" "test"\n+                byLabel "Content" "Above fold.\\n***\\nBelow fold."\n+\n+            bodyContains "Above fold."\n+            bodyContains "Below fold."\n+        |]\n+\n+\n+        yit "posts blog post" $ [marked|\n+            login\n+\n+            postBlog (NewProjectBlogPostR "snowdrift") $ do\n+                byLabel "Post Title" "Test"\n+                byLabel "Post Handle" "test"\n+                byLabel "Content" "Above fold.\\n***\\nBelow fold."\n+\n+            Just route <- extractLocation\n+\n+            get $ decodeUtf8 route\n+\n+            statusIs 200\n+\n+            htmlAnyContain ".post" "Above fold."\n+            htmlNoneContain ".post" "Below fold."\n+\n+            get $ ProjectBlogPostR "snowdrift" "test"\n+\n+            htmlAnyContain ".post" "Above fold."\n+            htmlAnyContain ".post" "Below fold."\n+        |]\n+\n+        yit "loads the project blog - with blog post" $ [marked|\n+            login\n+\n+            get $ ProjectBlogR "snowdrift"\n+\n+            statusIs 200\n+\n+            htmlAnyContain ".post" "Above fold."\n+            htmlNoneContain ".post" "Below fold."\n+        |]\n+\n+    {-\n+        yit "loads the project page - with blog post" $ [marked|\n+            login\n+\n+            get $ ProjectR "snowdrift"\n+\n+            statusIs 200\n+\n+            htmlAllContain "#post" "Above fold."\n+            htmlNoneContain "#post" "Below fold."\n+        |]\n+    -}\n+\n+\n+\ndiff --git a/tests/DiscussionTest.hs b/tests/DiscussionTest.hs\nindex 43c6d55..aec9eac 100644\n--- a/tests/DiscussionTest.hs\n+++ b/tests/DiscussionTest.hs\n@@ -19,7 +19,7 @@ import Control.Monad\n \n discussionSpecs :: Spec\n discussionSpecs = do\n-    let postComment route stmts = do\n+    let postComment route stmts = [marked|\n             get route\n             statusIs 200\n \n@@ -35,19 +35,21 @@ discussionSpecs = do\n                 stmts\n \n             statusIsResp 302\n+        |]\n \n         getLatestCommentId = do\n             [ Value (Just comment_id) ] <- testDB $ select $ from $ \\ comment -> return (max_ $ comment ^. CommentId)\n             return comment_id\n \n     ydescribe "discussion" $ do\n-        yit "loads the discussion page" $ do\n+        yit "loads the discussion page" $ [marked|\n             login\n \n             get $ DiscussWikiR "snowdrift" "about"\n             statusIs 200\n+        |]\n \n-        yit "posts and moves some comments" $ do\n+        yit "posts and moves some comments" $ [marked|\n             login\n \n             liftIO $ putStrLn "posting root comment"\n@@ -59,7 +61,7 @@ discussionSpecs = do\n             comment_map <- fmap M.fromList $ forM [1..10] $ \\ i -> do\n                 comment_id <- getLatestCommentId\n \n-                postComment (ReplyCommentR "snowdrift" "about" comment_id) $ byLabel "Reply" $ T.pack $ "Thread 1 - reply " ++ show i\n+                postComment (ReplyCommentR "snowdrift" "about" comment_id) $ byLabel "Reply" $ T.pack $ "Thread 1 - reply " ++ show (i :: Integer)\n \n                 return (i, comment_id)\n \n@@ -78,18 +80,20 @@ discussionSpecs = do\n                 addPostParam "mode" "rethread"\n \n             statusIsResp 302\n+        |]\n \n \n     ydescribe "discussion - rethreading" $ do\n-        let createComments = do\n+        let createComments = [marked|\n                 postComment (NewDiscussWikiR "snowdrift" "about") $ byLabel "New Topic" "First message"\n                 first <- getLatestCommentId\n                 postComment (NewDiscussWikiR "snowdrift" "about") $ byLabel "New Topic" "Second message"\n                 second <- getLatestCommentId\n \n                 return (first, second)\n+            |]\n \n-            testRethread first second = do\n+            testRethread first second = [marked|\n                 let rethread_url c = RethreadWikiCommentR "snowdrift" "about" c\n \n                 get $ rethread_url first\n@@ -112,9 +116,10 @@ discussionSpecs = do\n \n                 bodyContains "First message"\n                 bodyContains "Second message"\n+            |]\n \n \n-        yit "can move newer comments under older" $ do\n+        yit "can move newer comments under older" $ [marked|\n             login\n \n             get $ NewDiscussWikiR "snowdrift" "about"\n@@ -123,9 +128,10 @@ discussionSpecs = do\n             (first, second) <- createComments\n \n             testRethread first second\n+        |]\n \n \n-        yit "can move older comments under newer" $ do\n+        yit "can move older comments under newer" $ [marked|\n             login\n \n             get $ NewDiscussWikiR "snowdrift" "about"\n@@ -134,8 +140,9 @@ discussionSpecs = do\n             (first, second) <- createComments\n \n             testRethread second first\n+        |]\n \n-        yit "can rethread across pages and the redirect still works" $ do\n+        yit "can rethread across pages and the redirect still works" $ [marked|\n             login\n \n             postComment (NewDiscussWikiR "snowdrift" "about") $ byLabel "New Topic" "posting on about page"\n@@ -168,5 +175,6 @@ discussionSpecs = do\n                 desired_url = "http://localhost:3000/p/snowdrift/w/intro/c/" ++ (\\ (PersistInt64 i) -> show i) (unKey newId)\n \n             assertEqual ("Redirect not matching! (" ++ show new_url ++ " /=  " ++ show desired_url ++ ")") new_url desired_url\n+        |]\n \n \ndiff --git a/tests/TestImport.hs b/tests/TestImport.hs\nindex 5387a3a..2a8dda6 100644\n--- a/tests/TestImport.hs\n+++ b/tests/TestImport.hs\n@@ -1,5 +1,6 @@\n {-# LANGUAGE OverloadedStrings #-}\n {-# LANGUAGE FlexibleContexts #-}\n+{-# LANGUAGE TemplateHaskell #-}\n \n module TestImport\n     ( runDB\n@@ -10,6 +11,7 @@ module TestImport\n     , liftIO\n     , extractLocation\n     , statusIsResp\n+    , onException\n     , module TestImport\n     ) where\n \n@@ -36,6 +38,19 @@ import Model as TestImport\n \n import Control.Monad (when)\n \n+import qualified Language.Haskell.Meta.Parse as Exp\n+import qualified Language.Haskell.Meta.Syntax.Translate as Exp\n+import qualified Language.Haskell.TH as TH\n+import           Language.Haskell.TH.Quote\n+\n+import qualified Language.Haskell.Exts.Parser as Src\n+import qualified Language.Haskell.Exts.SrcLoc as Src\n+import qualified Language.Haskell.Exts.Pretty as Src\n+import qualified Language.Haskell.Exts.Annotated.Syntax as Src\n+\n+import Control.Exception.Lifted\n+\n+\n \n type Spec = YesodSpec App\n type Example = YesodExample App\n@@ -87,7 +102,6 @@ submitLogin user pass = do\n         addPostParam "username" user\n         addPostParam "password" pass\n \n-    extractLocation >>= liftIO . print\n \n extractLocation :: YesodExample site (Maybe B.ByteString)\n extractLocation = do\n@@ -110,14 +124,10 @@ needsLogin method url = do\n --\n login :: (Yesod site) => YesodExample site ()\n login = do\n-    liftIO $ putStrLn "Logging in..."\n     get $ urlPath $ testRoot `T.append` "/auth/login"\n     statusIs 200\n-    liftIO $ putStrLn "Submitting login."\n     submitLogin "test" "test"\n \n-    liftIO $ putStrLn "Logged in."\n-\n \n statusIsResp :: Int -> YesodExample site ()\n statusIsResp number = withResponse $ \\ SResponse { simpleStatus = s } -> do\n@@ -127,3 +137,39 @@ statusIsResp number = withResponse $ \\ SResponse { simpleStatus = s } -> do\n     , " but received status was ", show $ H.statusCode s\n     ]\n \n+\n+marked :: QuasiQuoter\n+marked = QuasiQuoter { quoteExp = decorate }\n+  where\n+    decorate input = do\n+        loc <- TH.location\n+        let file = TH.loc_filename loc\n+            (line, _) = TH.loc_start loc\n+\n+            fixup 1 = 0\n+            fixup x = x - 2\n+\n+            onException l = Src.QVarOp l $ Src.Qual l (Src.ModuleName l "TestImport") (Src.Ident l "onException")\n+            report l =\n+                let str = file ++ ":" ++ show (line + fixup (Src.srcLine l)) ++ ": exception raised here"\n+                 in Src.App l\n+                        (Src.Var l $ Src.Qual l (Src.ModuleName l "TestImport") (Src.Ident l "liftIO"))\n+                      $ Src.App l\n+                            (Src.Var l $ Src.Qual l (Src.ModuleName l "Prelude") (Src.Ident l "putStrLn"))\n+                            (Src.Lit l $ Src.String l str str)\n+\n+            mark l e = Src.InfixApp l (Src.Paren l e) (onException l) (report l)\n+\n+            decorateExp :: Src.Exp Src.SrcLoc -> Src.Exp Src.SrcLoc\n+            decorateExp (Src.Do l stmts) = mark l $ Src.Do l $ map decorateStmt stmts\n+            decorateExp exp = mark (Src.ann exp) exp\n+\n+            decorateStmt :: Src.Stmt Src.SrcLoc -> Src.Stmt Src.SrcLoc\n+            decorateStmt (Src.Generator l pat exp) = Src.Generator l pat $ decorateExp exp\n+            decorateStmt (Src.Qualifier l exp) = Src.Qualifier l $ decorateExp exp\n+            decorateStmt stmt = stmt\n+\n+        case Src.parse ("do\\n" ++ input) of\n+            Src.ParseOk a -> either fail return $ Exp.parseExp $ Src.prettyPrint $ decorateExp a\n+            Src.ParseFailed l e -> fail e\n+\ndiff --git a/tests/WikiTest.hs b/tests/WikiTest.hs\nindex 6c1a120..1f6df59 100644\n--- a/tests/WikiTest.hs\n+++ b/tests/WikiTest.hs\n@@ -4,21 +4,12 @@ module WikiTest\n     ) where\n \n import TestImport\n-import qualified Data.Map as M\n-import qualified Text.XML as XML\n-import qualified Text.HTML.DOM as HTML\n-\n-import Database.Esqueleto hiding (get)\n-\n-import Data.Text as T\n-\n-import Control.Monad\n \n wikiSpecs :: Spec\n wikiSpecs =\n     ydescribe "wiki" $ do\n \n-        yit "creates a new page" $ do\n+        yit "creates a new page" $ [marked|\n \n             login\n \n@@ -39,4 +30,5 @@ wikiSpecs =\n                 setMethod "POST"\n                 byLabel "Page Content" "test"\n                 addPostParam "mode" "post"\n+        |]\n \ndiff --git a/tests/main.hs b/tests/main.hs\nindex db3ce42..0f890f2 100644\n--- a/tests/main.hs\n+++ b/tests/main.hs\n@@ -13,6 +13,7 @@ import Application (makeFoundation)\n \n import DiscussionTest\n import WikiTest\n+import BlogTest\n \n import System.IO\n \n@@ -35,3 +36,5 @@ main = do\n \n             discussionSpecs\n \n+            blogSpecs\n+\n
\.


--
-- Name: build_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('build_id_seq', 25, true);


--
-- Data for Name: comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY comment (id, created_ts, approved_ts, approved_by, parent, "user", text, depth, discussion) FROM stdin;
1	2014-01-21 18:11:03.914397	2014-01-21 18:12:36.696658	1	\N	1	This is a comment.	0	2
2	2014-01-21 18:13:00.273315	2014-01-21 18:13:10.464805	1	1	1	Replies are threaded.	1	2
3	2014-01-21 18:13:57.732222	\N	\N	\N	1	When a comment is posted by an unestablished user, it is marked for moderation and only shown to moderators.	0	2
4	2014-01-21 18:15:30.945499	2014-01-21 18:15:37.484472	1	\N	1	adding a line starting with "ticket:" such as\n\nticket: this is a ticket\n\nmakes the post show up at /t where all the tickets are listed	0	2
\.


--
-- Data for Name: comment_ancestor; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY comment_ancestor (id, comment, ancestor) FROM stdin;
1	2	1
\.


--
-- Name: comment_ancestor_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('comment_ancestor_id_seq', 1, true);


--
-- Data for Name: comment_closure; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY comment_closure (id, ts, reason, comment, closed_by, type) FROM stdin;
\.


--
-- Data for Name: comment_flagging; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY comment_flagging (id, ts, flagger, comment, message) FROM stdin;
\.


--
-- Name: comment_flagging_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('comment_flagging_id_seq', 1, false);


--
-- Data for Name: comment_flagging_reason; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY comment_flagging_reason (id, flagging, reason) FROM stdin;
\.


--
-- Name: comment_flagging_reason_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('comment_flagging_reason_id_seq', 1, false);


--
-- Name: comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('comment_id_seq', 4, true);


--
-- Data for Name: comment_rethread; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY comment_rethread (id, rethread, old_comment, new_comment) FROM stdin;
\.


--
-- Name: comment_rethread_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('comment_rethread_id_seq', 1, false);


--
-- Data for Name: comment_retraction; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY comment_retraction (id, ts, reason, comment) FROM stdin;
\.


--
-- Name: comment_retraction_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('comment_retraction_id_seq', 1, false);


--
-- Name: comment_retraction_id_seq1; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('comment_retraction_id_seq1', 1, false);


--
-- Data for Name: comment_tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY comment_tag (id, comment, tag, "user", count) FROM stdin;
\.


--
-- Name: comment_tag_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('comment_tag_id_seq', 1, false);


--
-- Data for Name: committee_user; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY committee_user (id, created_ts, "user", project) FROM stdin;
\.


--
-- Name: committee_user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('committee_user_id_seq', 1, false);


--
-- Data for Name: database_version; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY database_version (id, last_migration) FROM stdin;
1	22
\.


--
-- Name: database_version_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('database_version_id_seq', 1, true);


--
-- Data for Name: default_tag_color; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY default_tag_color (id, tag, color) FROM stdin;
\.


--
-- Name: default_tag_color_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('default_tag_color_id_seq', 1, false);


--
-- Data for Name: discussion; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY discussion (id, nothing) FROM stdin;
1	0
2	0
3	0
4	0
5	0
6	0
7	0
\.


--
-- Name: discussion_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('discussion_id_seq', 7, true);


--
-- Data for Name: doc; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY doc (id, name, current_version) FROM stdin;
\.


--
-- Data for Name: doc_event; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY doc_event (id, "time", doc, blessed_version) FROM stdin;
\.


--
-- Name: doc_event_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('doc_event_id_seq', 1, false);


--
-- Name: doc_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('doc_id_seq', 1, false);


--
-- Data for Name: event_comment_pending; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY event_comment_pending (id, comment, ts) FROM stdin;
\.


--
-- Name: event_comment_pending_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('event_comment_pending_id_seq', 1, false);


--
-- Data for Name: event_comment_posted; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY event_comment_posted (id, comment, ts) FROM stdin;
\.


--
-- Name: event_comment_posted_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('event_comment_posted_id_seq', 1, false);


--
-- Data for Name: event_deleted_pledge; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY event_deleted_pledge (id, ts, "user", project, shares) FROM stdin;
\.


--
-- Name: event_deleted_pledge_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('event_deleted_pledge_id_seq', 1, false);


--
-- Data for Name: event_new_pledge; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY event_new_pledge (id, ts, shares_pledged) FROM stdin;
\.


--
-- Name: event_new_pledge_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('event_new_pledge_id_seq', 1, false);


--
-- Data for Name: event_notification_sent; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY event_notification_sent (id, ts, notification) FROM stdin;
\.


--
-- Name: event_notification_sent_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('event_notification_sent_id_seq', 1, false);


--
-- Data for Name: event_updated_pledge; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY event_updated_pledge (id, ts, old_shares, shares_pledged) FROM stdin;
\.


--
-- Name: event_updated_pledge_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('event_updated_pledge_id_seq', 1, false);


--
-- Data for Name: event_wiki_edit; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY event_wiki_edit (id, wiki_edit, ts) FROM stdin;
\.


--
-- Name: event_wiki_edit_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('event_wiki_edit_id_seq', 1, false);


--
-- Data for Name: event_wiki_page; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY event_wiki_page (id, ts, wiki_page) FROM stdin;
\.


--
-- Name: event_wiki_page_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('event_wiki_page_id_seq', 1, false);


--
-- Data for Name: interest; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY interest (id, description) FROM stdin;
\.


--
-- Name: interest_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('interest_id_seq', 1, false);


--
-- Data for Name: invite; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY invite (id, created_ts, project, code, "user", role, tag, redeemed, redeemed_ts, redeemed_by) FROM stdin;
1	2014-01-21 18:12:09.148007	1	df0176d67f1a4063	1	Moderator	admin as also moderator	t	2014-01-21 18:12:24.376052	1
2	2014-01-24 23:33:43.323505	1	e3ed7c9e1500fc54	1	TeamMember	admin as also team member	t	2014-01-24 23:33:53.481901	1
\.


--
-- Name: invite_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('invite_id_seq', 2, true);


--
-- Data for Name: manual_establishment; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY manual_establishment (id, established_user, establishing_user) FROM stdin;
\.


--
-- Name: manual_establishment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('manual_establishment_id_seq', 1, false);


--
-- Data for Name: notification; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY notification (id, created_ts, type, "to", project, content, archived) FROM stdin;
\.


--
-- Name: notification_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('notification_id_seq', 1, false);


--
-- Data for Name: payday; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY payday (id, date) FROM stdin;
\.


--
-- Name: payday_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('payday_id_seq', 1, false);


--
-- Data for Name: pledge; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY pledge (id, "user", project, shares, funded_shares, created_ts) FROM stdin;
\.


--
-- Data for Name: pledge_form_rendered; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY pledge_form_rendered (id, ts, "order", project, "user") FROM stdin;
\.


--
-- Name: pledge_form_rendered_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('pledge_form_rendered_id_seq', 1, false);


--
-- Name: pledge_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('pledge_id_seq', 1, false);


--
-- Data for Name: project; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project (id, created_ts, name, handle, description, account, share_value, last_payday, github_repo, discussion) FROM stdin;
1	2013-11-23 11:52:54.632763	Snowdrift.coop	snowdrift	The Snowdrift.coop site is itself one of the projects.	2	0	\N	\N	7
\.


--
-- Data for Name: project_blog; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project_blog (id, "time", title, "user", top_content, project, bottom_content, discussion, handle) FROM stdin;
\.


--
-- Data for Name: project_blog_comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project_blog_comment (id, comment, blog) FROM stdin;
\.


--
-- Name: project_blog_comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_blog_comment_id_seq', 1, false);


--
-- Name: project_blog_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_blog_id_seq', 1, false);


--
-- Name: project_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_id_seq', 1, true);


--
-- Data for Name: project_last_update; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project_last_update (id, project, update) FROM stdin;
1	1	1
\.


--
-- Name: project_last_update_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_last_update_id_seq', 1, true);


--
-- Data for Name: project_tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project_tag (id, project, tag) FROM stdin;
1	1	1
\.


--
-- Name: project_tag_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_tag_id_seq', 1, true);


--
-- Data for Name: project_update; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project_update (id, updated_ts, project, author, description) FROM stdin;
1	2014-01-24 21:49:51.132962	1	1	MarkdownDiff [(F,"Snowdrift Project"),(S,"The Snowdrift.coop site is itself one of the projects.")]
\.


--
-- Name: project_update_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_update_id_seq', 1, true);


--
-- Data for Name: project_user_role; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project_user_role (id, project, "user", role) FROM stdin;
2	1	1	Admin
3	1	1	Moderator
4	1	1	TeamMember
\.


--
-- Name: project_user_role_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_user_role_id_seq', 4, true);


--
-- Data for Name: rethread; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY rethread (id, ts, moderator, old_comment, reason) FROM stdin;
\.


--
-- Name: rethread_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('rethread_id_seq', 1, false);


--
-- Data for Name: role_event; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY role_event (id, "time", "user", role, project, added) FROM stdin;
1	2014-01-21 10:12:24.376209	1	Moderator	1	t
2	2014-01-24 15:33:53.482076	1	TeamMember	1	t
\.


--
-- Name: role_event_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('role_event_id_seq', 2, true);


--
-- Data for Name: shares_pledged; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY shares_pledged (id, ts, "user", project, shares, render) FROM stdin;
\.


--
-- Name: shares_pledged_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('shares_pledged_id_seq', 1, false);


--
-- Data for Name: tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY tag (id, name) FROM stdin;
1	website
\.


--
-- Data for Name: tag_color; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY tag_color (id, tag, "user", color) FROM stdin;
\.


--
-- Name: tag_color_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('tag_color_id_seq', 1, false);


--
-- Name: tag_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('tag_id_seq', 1, true);


--
-- Data for Name: ticket; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY ticket (id, created_ts, name, comment, updated_ts) FROM stdin;
1	2014-01-21 18:15:30.945499	this is a ticket	4	2014-01-21 18:15:30.945499
\.


--
-- Name: ticket_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('ticket_id_seq', 1, true);


--
-- Data for Name: transaction; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY transaction (id, ts, credit, debit, amount, reason, info, payday) FROM stdin;
\.


--
-- Name: transaction_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('transaction_id_seq', 1, false);


--
-- Data for Name: unapproved_comment_notification; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY unapproved_comment_notification (id, comment, notification) FROM stdin;
\.


--
-- Name: unapproved_comment_notification_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('unapproved_comment_notification_id_seq', 1, false);


--
-- Data for Name: user; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY "user" (id, ident, hash, salt, name, account, avatar, blurb, statement, irc_nick, read_notifications, read_applications, created_ts, established, discussion) FROM stdin;
2	davidleothomas@gmail.com	\N	\N	\N	3	\N	\N	\N	\N	2014-03-02 05:32:31.934125	2014-03-02 05:32:31.934125	\N	EstUnestablished	5
1	admin	8bf2d491387febc07e5d8fd15a4140b28473566e	P^YTN3G:	Admin	1	\N	Admin is the name for the test user in our devDB database that comes with the code. Log in as admin with passphrase: admin	\N	\N	2014-01-21 22:58:23.380462	2013-11-23 19:31:18.982213	\N	EstEstablished 2014-01-24 15:28:15.681117 2014-01-24 15:28:15.681117 "default"	6
\.


--
-- Name: user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('user_id_seq', 2, true);


--
-- Data for Name: user_message_pref; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY user_message_pref (id, "user", type, delivery) FROM stdin;
\.


--
-- Name: user_message_pref_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('user_message_pref_id_seq', 1, false);


--
-- Data for Name: user_notification_pref; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY user_notification_pref (id, "user", type, delivery) FROM stdin;
\.


--
-- Name: user_notification_pref_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('user_notification_pref_id_seq', 1, false);


--
-- Data for Name: user_setting; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY user_setting (id, "user", setting, value) FROM stdin;
\.


--
-- Name: user_setting_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('user_setting_id_seq', 1, false);


--
-- Data for Name: user_watching_project; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY user_watching_project (id, "user", project) FROM stdin;
\.


--
-- Name: user_watching_project_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('user_watching_project_id_seq', 1, false);


--
-- Data for Name: view_comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY view_comment (id, "user", comment) FROM stdin;
\.


--
-- Name: view_comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('view_comment_id_seq', 1, false);


--
-- Data for Name: view_time; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY view_time (id, "user", project, type, "time") FROM stdin;
\.


--
-- Name: view_time_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('view_time_id_seq', 1, false);


--
-- Data for Name: view_wiki_edit; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY view_wiki_edit (id, "user", edit) FROM stdin;
\.


--
-- Name: view_wiki_edit_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('view_wiki_edit_id_seq', 1, false);


--
-- Data for Name: volunteer_application; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY volunteer_application (id, created_ts, project, "user", name, email, other_contact_info, website, location, experience, comments) FROM stdin;
\.


--
-- Name: volunteer_application_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('volunteer_application_id_seq', 1, false);


--
-- Data for Name: volunteer_interest; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY volunteer_interest (id, volunteer, interest) FROM stdin;
\.


--
-- Name: volunteer_interest_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('volunteer_interest_id_seq', 1, false);


--
-- Data for Name: wiki_edit; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY wiki_edit (id, ts, "user", page, content, comment) FROM stdin;
1	2014-01-21 17:46:06.695166	1	1	# Welcome\n\nThank you for testing (and hopefully helping to develop) Snowdrift.coop!\n\nThis is a wiki page within your test database. It is different than the database for the real Snowdrift.coop site.	Page created.
2	2014-01-21 17:48:55.489319	1	2	# About Snowdrift.coop\n\nAll the real *about* stuff is on the live site: <https://snowdrift.coop/p/snowdrift/w/about>\n\nHere we will explain about testing.	Page created.
3	2014-01-21 17:52:33.270443	1	2	# About Snowdrift.coop\n\nAll the real *about* stuff is on the live site: <https://snowdrift.coop/p/snowdrift/w/about>\n\nHere we will explain about testing.\n\n## Wiki pages\n\nSee the live site for details about the wiki system: <https://snowdrift.coop/p/snowdrift/w/wiki>\n\nIn creating the page you are looking at, several edits were made, so you can click above to see the history.	Added links to wiki page on live site and comment about history
4	2014-01-21 17:53:21.094299	1	2	# About Snowdrift.coop\n\nAll the real *about* stuff is on the live site: <https://snowdrift.coop/p/snowdrift/w/about>\n\nHere we will explain about testing.\n\n## Wiki pages\n\nSee the live site for details about the wiki system: <https://snowdrift.coop/p/snowdrift/w/wiki>\n\nIn creating the page you are looking at, several edits were made, so you can click above to see the history.\n\nThere are discussion pages for every wiki page, as shown above.	Added sentence about discussion pages
5	2014-01-21 17:55:07.436846	1	3	See the live site for [press info](https://snowdrift.coop/p/snowdrift/w/press)	Page created.
6	2014-01-21 18:09:53.469506	1	4	# Development notes\n\nSee the live site for the full [how-to-help page](https://snowdrift.coop/p/snowdrift/w/how-to-help).\n\n## Development notes\n\nThe essential development details are in the README.md file with the code, not in this test database. When adding new info, consider whether it is best there versus here in the test database (the README has instructions about updating the test database).\n\n## Users\n\n[localhost:3000/u](/u) is a listing of all the users. The first user is just "admin" (passphrase is also "admin"). When new users register they start out unestablished and with no roles. You can add roles by using the admin user and visiting <http://localhost:3000/p/snowdrift/invite> and then logging in as another user to redeem the code.\n\nIt is a good idea to test things as:\n\na. logged-out\na. unestablished user\na. established users with different roles\n\nObviously testing on different systems, browsers, devices, etc. is good too.\n\n## Tickets\n\nSee <https://snowdrift.coop/p/snowdrift/t> for the live site's list of tickets. This is also linked at the live site's how-to-help page. Please add tickets to the live site as appropriate, add comments and questions, and mark things complete after you have fixed them and committed your changes.	Page created.
\.


--
-- Name: wiki_edit_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('wiki_edit_id_seq', 6, true);


--
-- Data for Name: wiki_last_edit; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY wiki_last_edit (id, page, edit) FROM stdin;
1	1	1
2	2	4
3	3	5
4	4	6
\.


--
-- Name: wiki_last_edit_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('wiki_last_edit_id_seq', 4, true);


--
-- Data for Name: wiki_page; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY wiki_page (id, target, project, content, permission_level, discussion, created_ts) FROM stdin;
1	intro	1	# Welcome\n\nThank you for testing (and hopefully helping to develop) Snowdrift.coop!\n\nThis is a wiki page within your test database. It is different than the database for the real Snowdrift.coop site.	Normal	1	2014-08-25 17:39:10.084581
2	about	1	# About Snowdrift.coop\n\nAll the real *about* stuff is on the live site: <https://snowdrift.coop/p/snowdrift/w/about>\n\nHere we will explain about testing.\n\n## Wiki pages\n\nSee the live site for details about the wiki system: <https://snowdrift.coop/p/snowdrift/w/wiki>\n\nIn creating the page you are looking at, several edits were made, so you can click above to see the history.\n\nThere are discussion pages for every wiki page, as shown above.	Normal	2	2014-08-25 17:39:10.084581
3	press	1	See the live site for [press info](https://snowdrift.coop/p/snowdrift/w/press)	Normal	3	2014-08-25 17:39:10.084581
4	how-to-help	1	# Development notes\n\nSee the live site for the full [how-to-help page](https://snowdrift.coop/p/snowdrift/w/how-to-help).\n\n## Development notes\n\nThe essential development details are in the README.md file with the code, not in this test database. When adding new info, consider whether it is best there versus here in the test database (the README has instructions about updating the test database).\n\n## Users\n\n[localhost:3000/u](/u) is a listing of all the users. The first user is just "admin" (passphrase is also "admin"). When new users register they start out unestablished and with no roles. You can add roles by using the admin user and visiting <http://localhost:3000/p/snowdrift/invite> and then logging in as another user to redeem the code.\n\nIt is a good idea to test things as:\n\na. logged-out\na. unestablished user\na. established users with different roles\n\nObviously testing on different systems, browsers, devices, etc. is good too.\n\n## Tickets\n\nSee <https://snowdrift.coop/p/snowdrift/t> for the live site's list of tickets. This is also linked at the live site's how-to-help page. Please add tickets to the live site as appropriate, add comments and questions, and mark things complete after you have fixed them and committed your changes.	Normal	4	2014-08-25 17:39:10.084581
\.


--
-- Data for Name: wiki_page_comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY wiki_page_comment (id, comment, page) FROM stdin;
1	1	2
2	2	2
3	3	2
4	4	2
\.


--
-- Name: wiki_page_comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('wiki_page_comment_id_seq', 4, true);


--
-- Name: wiki_page_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('wiki_page_id_seq', 4, true);


--
-- Name: account_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY account
    ADD CONSTRAINT account_pkey PRIMARY KEY (id);


--
-- Name: build_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY build
    ADD CONSTRAINT build_pkey PRIMARY KEY (id);


--
-- Name: comment_ancestor_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT comment_ancestor_pkey PRIMARY KEY (id);


--
-- Name: comment_flagging_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_flagging
    ADD CONSTRAINT comment_flagging_pkey PRIMARY KEY (id);


--
-- Name: comment_flagging_reason_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_flagging_reason
    ADD CONSTRAINT comment_flagging_reason_pkey PRIMARY KEY (id);


--
-- Name: comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_pkey PRIMARY KEY (id);


--
-- Name: comment_rethread_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_pkey PRIMARY KEY (id);


--
-- Name: comment_retraction_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_closure
    ADD CONSTRAINT comment_retraction_pkey PRIMARY KEY (id);


--
-- Name: comment_retraction_pkey1; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_retraction
    ADD CONSTRAINT comment_retraction_pkey1 PRIMARY KEY (id);


--
-- Name: comment_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_pkey PRIMARY KEY (id);


--
-- Name: committee_user_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT committee_user_pkey PRIMARY KEY (id);


--
-- Name: database_version_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY database_version
    ADD CONSTRAINT database_version_pkey PRIMARY KEY (id);


--
-- Name: default_tag_color_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY default_tag_color
    ADD CONSTRAINT default_tag_color_pkey PRIMARY KEY (id);


--
-- Name: discussion_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY discussion
    ADD CONSTRAINT discussion_pkey PRIMARY KEY (id);


--
-- Name: doc_event_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY doc_event
    ADD CONSTRAINT doc_event_pkey PRIMARY KEY (id);


--
-- Name: doc_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY doc
    ADD CONSTRAINT doc_pkey PRIMARY KEY (id);


--
-- Name: event_comment_pending_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY event_comment_pending
    ADD CONSTRAINT event_comment_pending_pkey PRIMARY KEY (id);


--
-- Name: event_comment_posted_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY event_comment_posted
    ADD CONSTRAINT event_comment_posted_pkey PRIMARY KEY (id);


--
-- Name: event_deleted_pledge_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY event_deleted_pledge
    ADD CONSTRAINT event_deleted_pledge_pkey PRIMARY KEY (id);


--
-- Name: event_new_pledge_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY event_new_pledge
    ADD CONSTRAINT event_new_pledge_pkey PRIMARY KEY (id);


--
-- Name: event_notification_sent_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY event_notification_sent
    ADD CONSTRAINT event_notification_sent_pkey PRIMARY KEY (id);


--
-- Name: event_updated_pledge_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY event_updated_pledge
    ADD CONSTRAINT event_updated_pledge_pkey PRIMARY KEY (id);


--
-- Name: event_wiki_edit_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY event_wiki_edit
    ADD CONSTRAINT event_wiki_edit_pkey PRIMARY KEY (id);


--
-- Name: event_wiki_page_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY event_wiki_page
    ADD CONSTRAINT event_wiki_page_pkey PRIMARY KEY (id);


--
-- Name: interest_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY interest
    ADD CONSTRAINT interest_pkey PRIMARY KEY (id);


--
-- Name: invite_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_pkey PRIMARY KEY (id);


--
-- Name: manual_establishment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY manual_establishment
    ADD CONSTRAINT manual_establishment_pkey PRIMARY KEY (id);


--
-- Name: notification_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY notification
    ADD CONSTRAINT notification_pkey PRIMARY KEY (id);


--
-- Name: payday_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY payday
    ADD CONSTRAINT payday_pkey PRIMARY KEY (id);


--
-- Name: pledge_form_rendered_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY pledge_form_rendered
    ADD CONSTRAINT pledge_form_rendered_pkey PRIMARY KEY (id);


--
-- Name: pledge_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT pledge_pkey PRIMARY KEY (id);


--
-- Name: project_blog_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT project_blog_comment_pkey PRIMARY KEY (id);


--
-- Name: project_blog_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_blog
    ADD CONSTRAINT project_blog_pkey PRIMARY KEY (id);


--
-- Name: project_last_update_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT project_last_update_pkey PRIMARY KEY (id);


--
-- Name: project_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_pkey PRIMARY KEY (id);


--
-- Name: project_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT project_tag_pkey PRIMARY KEY (id);


--
-- Name: project_update_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_update
    ADD CONSTRAINT project_update_pkey PRIMARY KEY (id);


--
-- Name: project_user_role_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT project_user_role_pkey PRIMARY KEY (id);


--
-- Name: rethread_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY rethread
    ADD CONSTRAINT rethread_pkey PRIMARY KEY (id);


--
-- Name: role_event_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY role_event
    ADD CONSTRAINT role_event_pkey PRIMARY KEY (id);


--
-- Name: shares_pledged_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY shares_pledged
    ADD CONSTRAINT shares_pledged_pkey PRIMARY KEY (id);


--
-- Name: tag_color_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT tag_color_pkey PRIMARY KEY (id);


--
-- Name: tag_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY tag
    ADD CONSTRAINT tag_pkey PRIMARY KEY (id);


--
-- Name: ticket_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY ticket
    ADD CONSTRAINT ticket_pkey PRIMARY KEY (id);


--
-- Name: transaction_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_pkey PRIMARY KEY (id);


--
-- Name: unapproved_comment_notification_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY unapproved_comment_notification
    ADD CONSTRAINT unapproved_comment_notification_pkey PRIMARY KEY (id);


--
-- Name: unique_comment_ancestor; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT unique_comment_ancestor UNIQUE (comment, ancestor);


--
-- Name: unique_comment_flagging; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_flagging
    ADD CONSTRAINT unique_comment_flagging UNIQUE (comment);


--
-- Name: unique_comment_flagging_reason; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_flagging_reason
    ADD CONSTRAINT unique_comment_flagging_reason UNIQUE (flagging, reason);


--
-- Name: unique_comment_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT unique_comment_tag UNIQUE (comment, tag, "user");


--
-- Name: unique_committee_member; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT unique_committee_member UNIQUE ("user");


--
-- Name: unique_default_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY default_tag_color
    ADD CONSTRAINT unique_default_tag UNIQUE (tag);


--
-- Name: unique_doc_name; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY doc
    ADD CONSTRAINT unique_doc_name UNIQUE (name);


--
-- Name: unique_invite; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT unique_invite UNIQUE (code);


--
-- Name: unique_manual_establishment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY manual_establishment
    ADD CONSTRAINT unique_manual_establishment UNIQUE (established_user);


--
-- Name: unique_pledge; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT unique_pledge UNIQUE ("user", project);


--
-- Name: unique_project_account; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project
    ADD CONSTRAINT unique_project_account UNIQUE (account);


--
-- Name: unique_project_blog_comment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT unique_project_blog_comment UNIQUE (comment);


--
-- Name: unique_project_blog_post; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_blog
    ADD CONSTRAINT unique_project_blog_post UNIQUE (project, handle);


--
-- Name: unique_project_handle; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project
    ADD CONSTRAINT unique_project_handle UNIQUE (handle);


--
-- Name: unique_project_last_update; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT unique_project_last_update UNIQUE (project);


--
-- Name: unique_project_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT unique_project_tag UNIQUE (project, tag);


--
-- Name: unique_project_user_role; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT unique_project_user_role UNIQUE (project, "user", role);


--
-- Name: unique_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY tag
    ADD CONSTRAINT unique_tag UNIQUE (name);


--
-- Name: unique_tag_color; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT unique_tag_color UNIQUE (tag, "user");


--
-- Name: unique_ticket; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY ticket
    ADD CONSTRAINT unique_ticket UNIQUE (comment);


--
-- Name: unique_user; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT unique_user UNIQUE (ident);


--
-- Name: unique_user_account; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT unique_user_account UNIQUE (account);


--
-- Name: unique_user_message_pref; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY user_message_pref
    ADD CONSTRAINT unique_user_message_pref UNIQUE ("user", type);


--
-- Name: unique_user_notification_pref; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY user_notification_pref
    ADD CONSTRAINT unique_user_notification_pref UNIQUE ("user", type);


--
-- Name: unique_user_watching_project; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY user_watching_project
    ADD CONSTRAINT unique_user_watching_project UNIQUE ("user", project);


--
-- Name: unique_view_comment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY view_comment
    ADD CONSTRAINT unique_view_comment UNIQUE ("user", comment);


--
-- Name: unique_view_time_user_project_type; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY view_time
    ADD CONSTRAINT unique_view_time_user_project_type UNIQUE ("user", project, type);


--
-- Name: unique_view_wiki_edit; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY view_wiki_edit
    ADD CONSTRAINT unique_view_wiki_edit UNIQUE ("user", edit);


--
-- Name: unique_wiki_last_edit; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT unique_wiki_last_edit UNIQUE (page);


--
-- Name: unique_wiki_page_comment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT unique_wiki_page_comment UNIQUE (comment);


--
-- Name: unique_wiki_target; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT unique_wiki_target UNIQUE (project, target);


--
-- Name: user_message_pref_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY user_message_pref
    ADD CONSTRAINT user_message_pref_pkey PRIMARY KEY (id);


--
-- Name: user_notification_pref_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY user_notification_pref
    ADD CONSTRAINT user_notification_pref_pkey PRIMARY KEY (id);


--
-- Name: user_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);


--
-- Name: user_setting_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY user_setting
    ADD CONSTRAINT user_setting_pkey PRIMARY KEY (id);


--
-- Name: user_watching_project_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY user_watching_project
    ADD CONSTRAINT user_watching_project_pkey PRIMARY KEY (id);


--
-- Name: view_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY view_comment
    ADD CONSTRAINT view_comment_pkey PRIMARY KEY (id);


--
-- Name: view_time_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY view_time
    ADD CONSTRAINT view_time_pkey PRIMARY KEY (id);


--
-- Name: view_wiki_edit_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY view_wiki_edit
    ADD CONSTRAINT view_wiki_edit_pkey PRIMARY KEY (id);


--
-- Name: volunteer_application_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY volunteer_application
    ADD CONSTRAINT volunteer_application_pkey PRIMARY KEY (id);


--
-- Name: volunteer_interest_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY volunteer_interest
    ADD CONSTRAINT volunteer_interest_pkey PRIMARY KEY (id);


--
-- Name: wiki_edit_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_edit
    ADD CONSTRAINT wiki_edit_pkey PRIMARY KEY (id);


--
-- Name: wiki_last_edit_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT wiki_last_edit_pkey PRIMARY KEY (id);


--
-- Name: wiki_page_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT wiki_page_comment_pkey PRIMARY KEY (id);


--
-- Name: wiki_page_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT wiki_page_pkey PRIMARY KEY (id);


--
-- Name: doc_event; Type: TRIGGER; Schema: public; Owner: snowdrift_development
--

CREATE TRIGGER doc_event AFTER INSERT OR DELETE ON doc FOR EACH ROW EXECUTE PROCEDURE log_doc_event_trigger();


--
-- Name: role_event; Type: TRIGGER; Schema: public; Owner: snowdrift_development
--

CREATE TRIGGER role_event AFTER INSERT OR DELETE ON project_user_role FOR EACH ROW EXECUTE PROCEDURE log_role_event_trigger();


--
-- Name: comment_ancestor_ancestor_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT comment_ancestor_ancestor_fkey FOREIGN KEY (ancestor) REFERENCES comment(id);


--
-- Name: comment_ancestor_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT comment_ancestor_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_approved_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_approved_by_fkey FOREIGN KEY (approved_by) REFERENCES "user"(id);


--
-- Name: comment_closure_closed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_closure
    ADD CONSTRAINT comment_closure_closed_by_fkey FOREIGN KEY (closed_by) REFERENCES "user"(id);


--
-- Name: comment_closure_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_closure
    ADD CONSTRAINT comment_closure_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_closure_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_closure
    ADD CONSTRAINT comment_closure_user_fkey FOREIGN KEY (closed_by) REFERENCES "user"(id);


--
-- Name: comment_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: comment_flagging_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_flagging
    ADD CONSTRAINT comment_flagging_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_flagging_flagger_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_flagging
    ADD CONSTRAINT comment_flagging_flagger_fkey FOREIGN KEY (flagger) REFERENCES "user"(id);


--
-- Name: comment_flagging_reason_flagging_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_flagging_reason
    ADD CONSTRAINT comment_flagging_reason_flagging_fkey FOREIGN KEY (flagging) REFERENCES comment_flagging(id);


--
-- Name: comment_moderated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_moderated_by_fkey FOREIGN KEY (approved_by) REFERENCES "user"(id);


--
-- Name: comment_moderated_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_moderated_user_fkey FOREIGN KEY (approved_by) REFERENCES "user"(id);


--
-- Name: comment_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_parent_fkey FOREIGN KEY (parent) REFERENCES comment(id);


--
-- Name: comment_rethread_new_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_new_comment_fkey FOREIGN KEY (new_comment) REFERENCES comment(id);


--
-- Name: comment_rethread_old_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_old_comment_fkey FOREIGN KEY (old_comment) REFERENCES comment(id);


--
-- Name: comment_rethread_rethread_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_rethread_fkey FOREIGN KEY (rethread) REFERENCES rethread(id);


--
-- Name: comment_retraction_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_closure
    ADD CONSTRAINT comment_retraction_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_retraction_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_retraction
    ADD CONSTRAINT comment_retraction_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_tag_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_tag_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: comment_tag_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: comment_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: committee_user_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT committee_user_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: committee_user_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT committee_user_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: default_tag_color_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY default_tag_color
    ADD CONSTRAINT default_tag_color_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: doc_current_version_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY doc
    ADD CONSTRAINT doc_current_version_fkey FOREIGN KEY (current_version) REFERENCES wiki_edit(id);


--
-- Name: doc_event_blessed_version_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY doc_event
    ADD CONSTRAINT doc_event_blessed_version_fkey FOREIGN KEY (blessed_version) REFERENCES wiki_edit(id);


--
-- Name: doc_event_doc_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY doc_event
    ADD CONSTRAINT doc_event_doc_fkey FOREIGN KEY (doc) REFERENCES doc(id);


--
-- Name: event_comment_pending_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_comment_pending
    ADD CONSTRAINT event_comment_pending_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: event_comment_posted_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_comment_posted
    ADD CONSTRAINT event_comment_posted_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: event_deleted_pledge_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_deleted_pledge
    ADD CONSTRAINT event_deleted_pledge_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: event_deleted_pledge_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_deleted_pledge
    ADD CONSTRAINT event_deleted_pledge_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: event_new_pledge_shares_pledged_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_new_pledge
    ADD CONSTRAINT event_new_pledge_shares_pledged_fkey FOREIGN KEY (shares_pledged) REFERENCES shares_pledged(id);


--
-- Name: event_notification_sent_notification_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_notification_sent
    ADD CONSTRAINT event_notification_sent_notification_fkey FOREIGN KEY (notification) REFERENCES notification(id);


--
-- Name: event_updated_pledge_shares_pledged_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_updated_pledge
    ADD CONSTRAINT event_updated_pledge_shares_pledged_fkey FOREIGN KEY (shares_pledged) REFERENCES shares_pledged(id);


--
-- Name: event_wiki_edit_wiki_edit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_wiki_edit
    ADD CONSTRAINT event_wiki_edit_wiki_edit_fkey FOREIGN KEY (wiki_edit) REFERENCES wiki_edit(id);


--
-- Name: event_wiki_page_wiki_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY event_wiki_page
    ADD CONSTRAINT event_wiki_page_wiki_page_fkey FOREIGN KEY (wiki_page) REFERENCES wiki_page(id);


--
-- Name: invite_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: invite_redeemed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_redeemed_by_fkey FOREIGN KEY (redeemed_by) REFERENCES "user"(id);


--
-- Name: invite_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: manual_establishment_established_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY manual_establishment
    ADD CONSTRAINT manual_establishment_established_user_fkey FOREIGN KEY (established_user) REFERENCES "user"(id);


--
-- Name: manual_establishment_establishing_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY manual_establishment
    ADD CONSTRAINT manual_establishment_establishing_user_fkey FOREIGN KEY (establishing_user) REFERENCES "user"(id);


--
-- Name: notification_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY notification
    ADD CONSTRAINT notification_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: notification_to_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY notification
    ADD CONSTRAINT notification_to_fkey FOREIGN KEY ("to") REFERENCES "user"(id);


--
-- Name: pledge_form_rendered_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY pledge_form_rendered
    ADD CONSTRAINT pledge_form_rendered_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: pledge_form_rendered_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY pledge_form_rendered
    ADD CONSTRAINT pledge_form_rendered_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: pledge_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT pledge_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: pledge_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT pledge_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: project_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_account_fkey FOREIGN KEY (account) REFERENCES account(id);


--
-- Name: project_blog_comment_blog_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT project_blog_comment_blog_fkey FOREIGN KEY (blog) REFERENCES project_blog(id);


--
-- Name: project_blog_comment_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT project_blog_comment_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: project_blog_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_blog
    ADD CONSTRAINT project_blog_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: project_blog_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_blog
    ADD CONSTRAINT project_blog_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_blog_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_blog
    ADD CONSTRAINT project_blog_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: project_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: project_last_payday_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_last_payday_fkey FOREIGN KEY (last_payday) REFERENCES payday(id);


--
-- Name: project_last_update_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT project_last_update_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_last_update_update_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT project_last_update_update_fkey FOREIGN KEY (update) REFERENCES project_update(id);


--
-- Name: project_tag_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT project_tag_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_tag_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT project_tag_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: project_update_author_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_update
    ADD CONSTRAINT project_update_author_fkey FOREIGN KEY (author) REFERENCES "user"(id);


--
-- Name: project_update_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_update
    ADD CONSTRAINT project_update_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_user_role_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT project_user_role_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_user_role_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT project_user_role_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: rethread_moderator_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY rethread
    ADD CONSTRAINT rethread_moderator_fkey FOREIGN KEY (moderator) REFERENCES "user"(id);


--
-- Name: rethread_old_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY rethread
    ADD CONSTRAINT rethread_old_comment_fkey FOREIGN KEY (old_comment) REFERENCES comment(id);


--
-- Name: role_event_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY role_event
    ADD CONSTRAINT role_event_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: role_event_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY role_event
    ADD CONSTRAINT role_event_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: shares_pledged_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY shares_pledged
    ADD CONSTRAINT shares_pledged_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: shares_pledged_render_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY shares_pledged
    ADD CONSTRAINT shares_pledged_render_fkey FOREIGN KEY (render) REFERENCES pledge_form_rendered(id);


--
-- Name: shares_pledged_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY shares_pledged
    ADD CONSTRAINT shares_pledged_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: tag_color_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT tag_color_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: tag_color_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT tag_color_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: ticket_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY ticket
    ADD CONSTRAINT ticket_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: transaction_credit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_credit_fkey FOREIGN KEY (credit) REFERENCES account(id);


--
-- Name: transaction_debit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_debit_fkey FOREIGN KEY (debit) REFERENCES account(id);


--
-- Name: transaction_payday_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_payday_fkey FOREIGN KEY (payday) REFERENCES payday(id);


--
-- Name: unapproved_comment_notification_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY unapproved_comment_notification
    ADD CONSTRAINT unapproved_comment_notification_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: unapproved_comment_notification_notification_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY unapproved_comment_notification
    ADD CONSTRAINT unapproved_comment_notification_notification_fkey FOREIGN KEY (notification) REFERENCES notification(id);


--
-- Name: user_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_account_fkey FOREIGN KEY (account) REFERENCES account(id);


--
-- Name: user_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: user_message_pref_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY user_message_pref
    ADD CONSTRAINT user_message_pref_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: user_notification_pref_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY user_notification_pref
    ADD CONSTRAINT user_notification_pref_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: user_setting_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY user_setting
    ADD CONSTRAINT user_setting_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: user_watching_project_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY user_watching_project
    ADD CONSTRAINT user_watching_project_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: user_watching_project_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY user_watching_project
    ADD CONSTRAINT user_watching_project_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: view_comment_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY view_comment
    ADD CONSTRAINT view_comment_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: view_comment_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY view_comment
    ADD CONSTRAINT view_comment_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: view_time_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY view_time
    ADD CONSTRAINT view_time_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: view_time_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY view_time
    ADD CONSTRAINT view_time_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: view_wiki_edit_edit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY view_wiki_edit
    ADD CONSTRAINT view_wiki_edit_edit_fkey FOREIGN KEY (edit) REFERENCES wiki_edit(id);


--
-- Name: view_wiki_edit_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY view_wiki_edit
    ADD CONSTRAINT view_wiki_edit_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: volunteer_application_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY volunteer_application
    ADD CONSTRAINT volunteer_application_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: volunteer_application_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY volunteer_application
    ADD CONSTRAINT volunteer_application_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: volunteer_interest_interest_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY volunteer_interest
    ADD CONSTRAINT volunteer_interest_interest_fkey FOREIGN KEY (interest) REFERENCES interest(id);


--
-- Name: volunteer_interest_volunteer_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY volunteer_interest
    ADD CONSTRAINT volunteer_interest_volunteer_fkey FOREIGN KEY (volunteer) REFERENCES volunteer_application(id);


--
-- Name: wiki_edit_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_edit
    ADD CONSTRAINT wiki_edit_page_fkey FOREIGN KEY (page) REFERENCES wiki_page(id);


--
-- Name: wiki_edit_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_edit
    ADD CONSTRAINT wiki_edit_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: wiki_last_edit_edit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT wiki_last_edit_edit_fkey FOREIGN KEY (edit) REFERENCES wiki_edit(id);


--
-- Name: wiki_last_edit_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT wiki_last_edit_page_fkey FOREIGN KEY (page) REFERENCES wiki_page(id);


--
-- Name: wiki_page_comment_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT wiki_page_comment_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: wiki_page_comment_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT wiki_page_comment_page_fkey FOREIGN KEY (page) REFERENCES wiki_page(id);


--
-- Name: wiki_page_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT wiki_page_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: wiki_page_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT wiki_page_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

