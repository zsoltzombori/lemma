#!/bin/bash

DEBUG_LEMMA=False
DEBUG_PRETRAIN=False
DEBUG_DTERM=False
DEBUG_PROVE=False
if [ "$DEBUG_LEMMA" == "True" ]; then
    stderr_lemma="error_lemma.out"
    echo "" > $stderr_lemma
else
    stderr_lemma="/dev/null"
fi
if [ "$DEBUG_PRETRAIN" == "True" ]; then
    stderr_pretrain="error_pretrain.out"
    echo "" > $stderr_pretrain
else
    stderr_pretrain="/dev/null"
fi
if [ "$DEBUG_DTERM" == "True" ]; then
    stderr_dterm="error_dterm.out"
    echo "" > $stderr_dterm
else
    stderr_dterm="/dev/null"
fi
if [ "$DEBUG_PROVE" == "True" ]; then
    stderr_prove="error_prove.out"
    echo "" > $stderr_prove
else
    stderr_prove="/dev/null"
fi

function prove {
    curr_problem=$1
    curr_traindir=$2
    curr_dtermdir=$3
    
    trainfile=${curr_traindir}/${curr_problem}.pl
    mkdir -p "$(dirname "$trainfile")"

    # if this is not iteration 0, then there should be a lemma file available
    dtermfile="${curr_dtermdir}/${curr_problem}.pl"
    if [ -f "$dtermfile" ]; then
        lemma_flag="-i $dtermfile"
    else
        lemma_flag=""
    fi

    
    SCRIPT="./prolog_scripts/provecd.sh -g \"--timeout ${SECURITY_TIMEOUT}\" -p \"[timeout=$PROVER_TIMEOUT]\" -t \"$PROVECD_CONFIG\" -l \"[max_lemmas=100,add_subproof_lemmas]\" $lemma_flag $curr_problem $PROVER_TIMEOUT $PROVER_CONFIG"
    # echo $SCRIPT
    /usr/bin/timeout --preserve-status -k 5 ${SECURITY_TIMEOUT} ./prolog_scripts/provecd.sh -g "--timeout ${SECURITY_TIMEOUT}" -p "[timeout=$PROVER_TIMEOUT]" -t "$PROVECD_CONFIG" -l "[max_lemmas=100,add_subproof_lemmas]" $lemma_flag $curr_problem $PROVER_TIMEOUT $PROVER_CONFIG > $trainfile
}
export -f prove

function extract_lemma {
    curr_problem=$1
    curr_lemmadir=$2
    
    lemmafile=$curr_lemmadir/${curr_problem}.pl
    mkdir -p "$(dirname "$lemmafile")"
    ./prolog_scripts/lemgen.sh -o [short_ids] $curr_problem $LEMGEN_CONFIG > $lemmafile

    if grep -q "proof_found" "$lemmafile"; then
        rm $lemmafile
    fi

}
export -f extract_lemma

function extract_pretrain {
    curr_problem=$1
    curr_pretraindir=$2
    
    pretrainfile=$curr_pretraindir/${curr_problem}.pl
    mkdir -p "$(dirname "$pretrainfile")"
    eval ./prolog_scripts/lemgen.sh -o "$DATAGEN_CONFIG" $curr_problem $LEMGEN_CONFIG > $pretrainfile

    if grep -q "proof_found" "$pretrainfile"; then
        rm $pretrainfile
    fi
        
}
export -f extract_pretrain


function extract_dterm {
    curr_problem=$1
    curr_lemmadir=$2
    curr_dtermdir=$3
    curr_model_path=$4
    
    lemmafile=$curr_lemmadir/${curr_problem}.pl

    # only select best lemmas when no proof was found
    if [[ -f "$lemmafile" ]]   
    then
        dtermfile=${curr_dtermdir}/${curr_problem}.pl
        mkdir -p "$(dirname "$dtermfile")"

        if [ "$LEMSORT_TRAIN" == "1" ]; then
            python lemma_selector.py --saved_model_path $curr_model_path --lemma_file $lemmafile --utility $LEMMA_UTILITY --lemma_count $LEMMA_COUNT --out_type $LEMMA_FORMAT --outfile ${dtermfile} --convolutions $TRAIN_CONVOLUTIONS --channels $TRAIN_CHANNELS --hidden $TRAIN_HIDDEN --model_type $TRAIN_MODEL_TYPE --concat_linear $TRAIN_CONCAT_LINEAR
        elif [ "$LEMSORT_TRAIN" == "2" ]; then
            python lemma_selector.py --saved_model_path $OLD_MODEL --lemma_file $lemmafile --utility $LEMMA_UTILITY --lemma_count $LEMMA_COUNT --out_type $LEMMA_FORMAT --outfile ${dtermfile} --convolutions $TRAIN_CONVOLUTIONS --channels $TRAIN_CHANNELS --hidden $TRAIN_HIDDEN --model_type $TRAIN_MODEL_TYPE --concat_linear $TRAIN_CONCAT_LINEAR
        else
            ./prolog_scripts/spo-lemmas.sh -s \"$LEMSORT_DEFAULT\" -h $LEMMA_COUNT $lemmafile > ${dtermfile}
        fi
    fi  
}
export -f extract_dterm




#########################################################################################
# extract lemma candidates for each problem
lemmadir="lemmas/${LEMGEN_CONFIG}"

if [ ! -d "$lemmadir" ]; then
    mkdir -p $lemmadir
    
    # extract candidate lemmas for each problem
    TIME_LEMMA_START=$(date +%s)
    echo "---------------- LEMMA EXTRACTION"
    SCRIPT="extract_lemma {} $lemmadir 2> $stderr_lemma"
    cat $PROBLEM_FILE | shuf | parallel -j ${PROVER_CORENUM} --no-notice $SCRIPT
    TIME_LEMMA_END=$(date +%s)
    echo "   Lemma extraction time: $(( TIME_LEMMA_END - TIME_LEMMA_START )) sec"
fi


#########################################################################################
# if pretraining is allowed, extract pretrain dataset from the lemma generator
DATAGEN_HASH=`echo -n ${DATAGEN_CONFIG} | md5sum | awk '{print $1}'`
pretraindir="pretrain/${LEMGEN_CONFIG}/${DATAGEN_HASH}"
echo "PRETRAIN DIR: $pretraindir"
if [ "$DATAGEN" -ge "1" ]; then
    if [ ! -d "$pretraindir" ]; then
        mkdir -p $pretraindir
    
        # extract training data from lemma generator
        TIME_PRETRAIN_START=$(date +%s)
        echo "---------------- PRETRAINING EXTRACTION"
        SCRIPT="extract_pretrain {} $pretraindir 2> $stderr_pretrain"
        cat $PROBLEM_FILE | shuf | parallel -j ${PROVER_CORENUM} --no-notice $SCRIPT
        TIME_PRETRAIN_END=$(date +%s)
        echo "   Training data extraction from lemgen time: $(( TIME_PRETRAIN_END - TIME_PRETRAIN_START )) sec"
    fi

    # if we don't yet have a pretrained model, run pretraining
    pretrain_model="$pretraindir/model__${TRAIN_MODEL_TYPE}__${TRAIN_UTILITY}__${TRAIN_HIDDEN}__${TRAIN_CHANNELS}__${TRAIN_CONVOLUTIONS}__${TRAIN_CONCAT_LINEAR}"

    # if [ ! -f "$pretrain_model" ]; then
    #     TIME_TRAIN_START=$(date +%s)
    #     echo "------------------ PRETRAINING"
    #     SCRIPT="python train.py --validation 0.2 --bs $TRAIN_BS --lr $TRAIN_LR --tolerance $TRAIN_TOLERANCE --convolutions $TRAIN_CONVOLUTIONS --channels $TRAIN_CHANNELS --hidden $TRAIN_HIDDEN --data_dir $pretraindir --new_model_path $pretrain_model --utility $TRAIN_UTILITY --model_type $TRAIN_MODEL_TYPE --concat_linear $TRAIN_CONCAT_LINEAR"
    #     echo $SCRIPT
    #     eval $SCRIPT
    #     TIME_TRAIN_END=$(date +%s)
    #     echo "   Pretraining time: $(( TIME_TRAIN_END - TIME_TRAIN_START )) sec"
    # fi

    # extra_training_flag="--saved_model_path $pretrain_model"
    extra_training_flag="--data_dir_extra $pretraindir"
else
    extra_training_flag=""
fi


#########################################################################################
# start iterative proof search and learning



mkdir -p $EXPDIR

for iter in 0 1 2
do
    TIME_ITER_START=$(date +%s)
    echo ""
    echo ""
    echo "ITERATION $iter"
    
    
    iterdir=$EXPDIR/$iter
    traindir=$iterdir/train
    model_path=$iterdir/model
    dtermdir=$iterdir/dterm
    previous_iter=$((iter-1))
    previous_iterdir=$EXPDIR/${previous_iter}
    previous_traindir=${previous_iterdir}/train

    rm -rf $iterdir
    mkdir -p $iterdir
    mkdir -p $traindir

    #########################################################################################
    # if this is not iteration 0, train a model and extract dterms
    if [ "$iter" -ge "1" ]; then
        # only train if we are using a traned model for sorting lemmas
        if [ "$LEMSORT_TRAIN" == "1" ]; then
            if [ "$DATAGEN" -le "1" ]; then
                TIME_TRAIN_START=$(date +%s)
                echo "------------------ TRAINING"
                SCRIPT="python train.py --validation $TRAIN_VALIDATION --bs $TRAIN_BS --lr $TRAIN_LR --tolerance $TRAIN_TOLERANCE --convolutions $TRAIN_CONVOLUTIONS --channels $TRAIN_CHANNELS --hidden $TRAIN_HIDDEN --data_dir $previous_traindir --new_model_path $model_path --utility $TRAIN_UTILITY --model_type $TRAIN_MODEL_TYPE --concat_linear $TRAIN_CONCAT_LINEAR $extra_training_flag"
                echo $SCRIPT
                eval $SCRIPT
                TIME_TRAIN_END=$(date +%s)
                echo "   Training time: $(( TIME_TRAIN_END - TIME_TRAIN_START )) sec"
            else # we just copy over the pretrained model
                cp $pretrain_model $model_path
            fi
        fi


        # extract dterms from best lemmas
        mkdir -p $dtermdir
        TIME_DTERM_START=$(date +%s)
        echo "---------------- DTERM EXTRACTION"
        SCRIPT="extract_dterm {} $lemmadir $dtermdir $model_path 2>> $stderr_dterm"
        cat $PROBLEM_FILE | shuf | parallel -j ${LEMGEN_CORENUM} --no-notice $SCRIPT
        TIME_DTERM_END=$(date +%s)
        echo "   Dterm extraction time: $(( TIME_DTERM_END - TIME_DTERM_START )) sec"        
    fi

    
    #########################################################################################
    # loop through all problems and try to prove them
    TIME_PROVE_START=$(date +%s)
    echo "---------------- PROOF SEARCH"
    SCRIPT="prove {} $traindir $dtermdir 2>> $stderr_prove"
    cat $PROBLEM_FILE | shuf | parallel -j ${PROVER_CORENUM} --no-notice $SCRIPT
    TIME_PROVE_END=$(date +%s)
    echo "   Proving time: $(( TIME_PROVE_END - TIME_PROVE_START )) sec"


    # count the number of problems solved
    if [[ "${PROVER_CONFIG}" == *"eprover"* ]]; then
        solved=`find $traindir -type f -print0 | xargs -0 grep -l "proof_external" | wc -l`
        failed=`find $traindir -type f -print0 | xargs -0 grep -L "proof_external" | wc -l`
        TOTAL_SOLVED=`find ${EXPDIR}/*/train/ -type f -print0 | xargs -0 grep -l "proof_external" | xargs basename -a | sort | uniq | wc -l`
    else
        solved=`find $traindir -type f -not -empty | wc -l`
        failed=`find $traindir -type f -empty | wc -l`        
        TOTAL_SOLVED=`find ${EXPDIR}/*/train/ -type f -not -empty | xargs basename -a | sort | uniq | wc -l`
    fi

    echo "SOLVED $solved, FAILED $failed"
    echo "TOTAL PROBLEMS SOLVED: ${TOTAL_SOLVED}"

    find $traindir -type f -not -empty | xargs basename -a | sort > ${iterdir}/solved.tmp
    if [ "$iter" -ge "1" ]; then
        lost=`comm -23 ${previous_iterdir}/solved.tmp ${iterdir}/solved.tmp | wc -l`
        gained=`comm -13 ${previous_iterdir}/solved.tmp ${iterdir}/solved.tmp | wc -l`
        echo "GAINED $gained, LOST: $lost"
    fi
    #########################################################################################

    TIME_ITER_END=$(date +%s)
    echo "   Iteration time: $(( TIME_ITER_END - TIME_ITER_START )) sec"
done


TOTAL_SOLVED=`find ${EXPDIR}/*/train/ -type f -not -empty | xargs basename -a | sort | uniq | wc -l`
echo "TOTAL PROBLEMS SOLVED: ${TOTAL_SOLVED}"

